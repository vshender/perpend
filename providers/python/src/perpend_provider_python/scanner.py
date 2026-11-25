"""The pass over the syntax tree of one file.

It finds the imports of the file and the flags of their context.  It does not
resolve a module name to a file; the resolver does that.
"""

import ast
import io
import os
import re
import tokenize
import warnings
from dataclasses import dataclass, field
from typing import final, override

from perpend_provider_python.errors import ProviderError
from perpend_provider_python.records import Flag

LINE_BREAK = re.compile(r"\r\n|\r|\n")
"""The line breaks that the Python parser counts.

`str.splitlines` breaks on more characters, so its line numbers would differ
from the line numbers of the parser.
"""


@dataclass
class Import:
    """One imported module of an import statement: `import a, b` gives two."""

    line: int
    """The line of the statement, from 1."""

    module: str
    """The absolute name of the imported module."""

    written: str
    """The module as the statement writes it: the same as `module`, or a
    relative name with its dots, such as `..core`.
    """

    names: list[str]
    """What `from module import ...` brings in; empty for `import module` and
    for `from module import *`, which brings in names that the scan does not
    know.
    """

    flags: list[Flag]
    """The flags of the context of the statement."""


@dataclass
class DynamicImport:
    """A call that imports a module when the code runs."""

    line: int
    """The line of the call, from 1."""

    call: str
    """The full name of the function, such as `importlib.import_module`, even
    when the code calls it by the bare name `import_module`.
    """

    target: str | None
    """The module name, when it is the only argument of the call and a string
    literal; else `None`.
    """

    flags: list[Flag]
    """`Flag.DYNAMIC` and the flags of the context of the call."""


@dataclass
class ImportAboveTopLevel:
    """A relative import that goes above the top-level package: it has more
    leading dots than the package of the file has parts.

    Such an import names no module, and Python rejects it when it runs.
    """

    line: int
    """The line of the statement, from 1."""

    written: str
    """The module as the statement writes it, with its dots, such as `...x`."""


@dataclass
class Scan:
    """What the scan of one file found."""

    imports: list[Import] = field(default_factory=list)
    """The imports, in the order of the file."""

    dynamic_imports: list[DynamicImport] = field(default_factory=list)
    """The dynamic imports, in the order in which the walk visits the calls.
    It can differ from the order of the file: for example, the first iterable
    of a generator expression is visited before its element.
    """

    imports_above_top_level: list[ImportAboveTopLevel] = field(default_factory=list)
    """The relative imports that go above the top-level package, in the order
    of the file.
    """


@final
class Scanner(ast.NodeVisitor):
    """A walk over the syntax tree of one module that records its imports.

    Every import gets the flags of the context where it is written.  See
    `Flag` for the flags; the method that visits a construct says which flag
    the construct sets.
    """

    scan: Scan
    """What the scan found so far."""

    def __init__(self, package: str) -> None:
        """Create a scanner.

        Args:
            package: The absolute name of the package from which the relative
                imports of the module start.  It must not be empty: a relative
                import needs a package to start from.
        """

        self.package: str = package
        self.scan = Scan()

        # How deep the visited node is inside each construct that gives a flag.
        # A counter goes up on the way into the body and down on the way out,
        # so an import has the flag while its counter is above zero.  They are
        # counters, not booleans, because the constructs nest.
        #
        # On the way into code that runs later, the lazy counter goes up, and
        # the other two have rules of their own, see `_visit_lazy`:
        #
        # - The `try` counter starts from zero again: such code can run after
        #   the `try` around it has ended, and then the `try` does not catch
        #   what the code raises.
        # - The `TYPE_CHECKING` counter keeps its value: a function that is
        #   defined under `if TYPE_CHECKING:` does not exist at run time, so
        #   the imports in it are type-only too.
        self._lazy_depth: int = 0
        self._try_depth: int = 0
        self._type_checking_depth: int = 0

    def _flags(self) -> list[Flag]:
        """Return the flags of the current context in the tree."""

        flags: list[Flag] = []
        if self._type_checking_depth:
            flags.append(Flag.TYPE_ONLY)
        if self._lazy_depth:
            flags.append(Flag.LAZY)
        if self._try_depth:
            flags.append(Flag.GUARDED)

        return flags

    def _absolute(self, module: str | None, level: int) -> str | None:
        """Return the absolute name of the module that a `from` import names.

        Args:
            module: The name after the dots, or `None` for `from . import x`.
            level: The number of leading dots.

        Returns:
            The absolute name, or `None` when the import goes above the
            top-level package and so names no module.
        """

        if level == 0:
            return module or ""

        # One dot is the package itself and every next dot is one level up, so a
        # package whose name has N parts allows N dots.
        parts = self.package.split(".")
        if level > len(parts):
            return None

        base = parts[: len(parts) - (level - 1)]
        if module:
            base.append(module)

        return ".".join(base)

    def _visit_lazy(self, nodes: list[ast.AST]) -> None:
        """Visit `nodes`, code that runs later than the code around it.

        The imports in `nodes` are lazy.  A `try` around `nodes` does not guard
        them: their code can run after that `try` has ended.
        """

        try_depth, self._try_depth = self._try_depth, 0
        self._lazy_depth += 1
        for node in nodes:
            self.visit(node)
        self._lazy_depth -= 1
        self._try_depth = try_depth

    def _visit_function(self, node: ast.FunctionDef | ast.AsyncFunctionDef) -> None:
        """Visit a function and mark the imports in its body as lazy.

        A part of a function is lazy or not by when Python runs it:

        - The body is lazy: it runs when the function is called.
        - The expressions in the type parameters, such as `Bound` in
          `def f[T: Bound]`, are lazy: Python evaluates them only when something
          reads them, for example through `T.__bound__`.
        - The decorators and the default values are not lazy: they run when the
          function is defined.
        - The annotations are not lazy: before Python 3.14, and without
          `from __future__ import annotations`, they run when the function is
          defined, and the provider does not know which Python runs the code.
        """

        for decorator in node.decorator_list:
            self.visit(decorator)
        self.visit(node.args)
        if node.returns is not None:
            self.visit(node.returns)
        self._visit_lazy([*node.type_params, *node.body])

    def _visit_try(self, node: ast.Try | ast.TryStar) -> None:
        """Visit a `try` and mark the imports in its body as guarded.

        A part of a `try` is guarded or not by whether the `try` catches an
        exception that is raised in it:

        - The body is guarded when the `try` has a handler of any class, as
          `Flag.GUARDED` says.  A `try` with only `finally` catches nothing, so
          its body is not guarded.
        - The handlers, `else` and `finally` are not guarded by this `try`: it
          does not catch an exception that is raised in them.
        """

        guarded = 1 if node.handlers else 0
        self._try_depth += guarded
        for statement in node.body:
            self.visit(statement)
        self._try_depth -= guarded

        for handler in node.handlers:
            self.visit(handler)
        for statement in node.orelse + node.finalbody:
            self.visit(statement)

    @override
    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        """Visit a function definition."""

        self._visit_function(node)

    @override
    def visit_AsyncFunctionDef(self, node: ast.AsyncFunctionDef) -> None:
        """Visit an async function definition."""

        self._visit_function(node)

    @override
    def visit_Lambda(self, node: ast.Lambda) -> None:
        """Visit a lambda; its body is lazy, its default values are not."""

        self.visit(node.args)
        self._visit_lazy([node.body])

    @override
    def visit_ClassDef(self, node: ast.ClassDef) -> None:
        """Visit a class; its type parameters are lazy, its body is not.

        Python evaluates the expressions in the type parameters only when
        they are read, and runs the body when the class is defined.
        """

        for decorator in node.decorator_list:
            self.visit(decorator)
        for base in node.bases:
            self.visit(base)
        for keyword in node.keywords:
            self.visit(keyword)
        self._visit_lazy(list(node.type_params))
        for statement in node.body:
            self.visit(statement)

    @override
    def visit_TypeAlias(self, node: ast.TypeAlias) -> None:
        """Visit a `type` statement; its value and type parameters are lazy.

        Python evaluates the value and the expressions in the type parameters
        only when they are read.
        """

        self._visit_lazy([*node.type_params, node.value])

    @override
    def visit_GeneratorExp(self, node: ast.GeneratorExp) -> None:
        """Visit a generator expression; all but its first iterable is lazy.

        Python evaluates the first iterable when it creates the generator, and
        the rest when the generator is iterated.
        """

        first, *rest = node.generators
        self.visit(first.iter)
        self._visit_lazy([first.target, *first.ifs, *rest, node.elt])

    @override
    def visit_Try(self, node: ast.Try) -> None:
        """Visit a `try` statement."""

        self._visit_try(node)

    @override
    def visit_TryStar(self, node: ast.TryStar) -> None:
        """Visit a `try` statement with `except*`."""

        self._visit_try(node)

    @override
    def visit_If(self, node: ast.If) -> None:
        """Visit an `if`; the imports under `TYPE_CHECKING` are type-only.

        The body of an `if` is type-only when its condition is the bare name
        `TYPE_CHECKING` or an attribute with that name, such as
        `typing.TYPE_CHECKING`.  The `else` branch is not made type-only by this
        `if`.
        """

        self.visit(node.test)

        test = node.test
        type_checking = (isinstance(test, ast.Name) and test.id == "TYPE_CHECKING") or (
            isinstance(test, ast.Attribute) and test.attr == "TYPE_CHECKING"
        )

        if type_checking:
            self._type_checking_depth += 1
        for statement in node.body:
            self.visit(statement)
        if type_checking:
            self._type_checking_depth -= 1

        for statement in node.orelse:
            self.visit(statement)

    @override
    def visit_Import(self, node: ast.Import) -> None:
        """Record `import a, b` as one import per module."""

        for alias in node.names:
            self.scan.imports.append(
                Import(
                    line=node.lineno,
                    module=alias.name,
                    written=alias.name,
                    names=[],
                    flags=self._flags(),
                )
            )

    @override
    def visit_ImportFrom(self, node: ast.ImportFrom) -> None:
        """Record `from a import b, c` as one import with the names.

        A relative import that goes above the top-level package is recorded
        apart, as an `ImportAboveTopLevel`.
        """

        written = "." * node.level + (node.module or "")
        module = self._absolute(node.module, node.level)
        if module is None:
            self.scan.imports_above_top_level.append(
                ImportAboveTopLevel(line=node.lineno, written=written)
            )
            return

        names = [alias.name for alias in node.names if alias.name != "*"]
        self.scan.imports.append(
            Import(
                line=node.lineno,
                module=module,
                written=written,
                names=names,
                flags=self._flags(),
            )
        )

    @override
    def visit_Call(self, node: ast.Call) -> None:
        """Record a call of `importlib.import_module` or `__import__`.

        The call is recognised by the names in it:
        `importlib.import_module(...)`, a call of the bare name
        `import_module(...)`, and `__import__(...)`.  The provider does not
        check that the name `import_module` comes from `importlib`.  A call
        through another name, such as an alias of the module or of the
        function, is not recognised.

        The target is the module name when it is the only argument and a
        string literal, given by position or as `name=`.  A call with any
        argument besides the module name has no target: `package` or `level`
        can change which module the name means, and the provider does not
        interpret the other arguments.
        """

        function = node.func
        call = None
        if (
            isinstance(function, ast.Attribute)
            and function.attr == "import_module"
            and isinstance(function.value, ast.Name)
            and function.value.id == "importlib"
        ):
            call = "importlib.import_module"
        elif isinstance(function, ast.Name) and function.id == "import_module":
            call = "importlib.import_module"
        elif isinstance(function, ast.Name) and function.id == "__import__":
            call = "__import__"

        if call is not None:
            target = None
            arguments = [*node.args, *(k.value for k in node.keywords)]
            by_name = all(keyword.arg == "name" for keyword in node.keywords)
            if len(arguments) == 1 and by_name:
                argument = arguments[0]
                if isinstance(argument, ast.Constant) and isinstance(
                    argument.value, str
                ):
                    target = argument.value

            self.scan.dynamic_imports.append(
                DynamicImport(
                    line=node.lineno,
                    call=call,
                    target=target,
                    flags=[*self._flags(), Flag.DYNAMIC],
                )
            )

        self.generic_visit(node)


def scan_file(root: str, path: str, module: str) -> tuple[Scan, list[str]]:
    """Scan one file of the repository.

    A relative import starts from the package of the file: the module itself
    when the file is `__init__.py`, else the package that holds the module.

    Args:
        root: The repository root, an absolute path.
        path: The file, relative to the root.
        module: The absolute name of the module in the file.

    Returns:
        The scan and the lines of the file, for the evidence.  The lines are
        split as the parser counts them, and decoded with the encoding that the
        file declares, UTF-8 by default.

    Raises:
        ProviderError: The file cannot be read, does not parse, or has code
            that is nested too deeply to parse or to walk.
    """

    try:
        with open(os.path.join(root, path), "rb") as handle:
            source = handle.read()
    except OSError as error:
        raise ProviderError(f"{path}: {error.strerror}") from None

    try:
        # The parser warns about the code that it reads, for example about an
        # invalid escape in a string.  Such a warning is about the scanned code,
        # not about the provider, so it is not shown.
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", SyntaxWarning)
            tree = ast.parse(source, filename=path)
    except SyntaxError as error:
        raise syntax_error(path, error.lineno, error.text) from None
    except RecursionError:
        raise ProviderError(f"{path}: the code is nested too deeply") from None
    except MemoryError as error:
        # The parser raises `MemoryError` with this message when its own stack
        # for nested code is full.  Any other `MemoryError` means that the
        # process ran out of memory, which is not about the file.
        if not str(error).startswith("Parser stack overflowed"):
            raise
        raise ProviderError(f"{path}: the code is nested too deeply") from None

    is_package = os.path.basename(path) == "__init__.py"
    scanner = Scanner(module if is_package else module.rpartition(".")[0])
    try:
        scanner.visit(tree)
    except RecursionError:
        raise ProviderError(f"{path}: the code is nested too deeply") from None

    encoding, _ = tokenize.detect_encoding(io.BytesIO(source).readline)
    text = source.decode(encoding, errors="replace")

    return scanner.scan, LINE_BREAK.split(text)


def syntax_error(path: str, line: int | None, text: str | None) -> ProviderError:
    """Return the error for a syntax error in `path`.

    Args:
        path: The file, relative to the root.
        line: The line of the error, when known.
        text: The text of that line, when known.
    """

    message = f"{path}, line {line or '?'}: syntax error"
    if text and text.strip():
        message += f": {text.strip()}"

    return ProviderError(message)
