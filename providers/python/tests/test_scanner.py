"""Tests for `perpend_provider_python.scanner`."""

from pathlib import Path

from perpend_provider_python.records import Flag
from perpend_provider_python.scanner import scan_file
from tests.factories import make_dynamic, make_import, scan_source, write_files

# The scan of one file


def test_import_at_the_top_level_has_no_flags() -> None:
    """An import at the top level of a module has no flags."""

    # GIVEN a module with a plain import and a from-import
    source = "import os\nfrom app.core import models, storage\n"

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN each statement is recorded on its line with no flags
    assert scan.imports == [
        make_import("os", line=1),
        make_import("app.core", ["models", "storage"], line=2),
    ]


def test_import_in_a_function_is_lazy() -> None:
    """An import in a function or an async function is `lazy`."""

    # GIVEN imports inside a function and an async function
    source = "def f():\n    import os\nasync def g():\n    import re\n"

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN both are lazy
    assert scan.imports == [
        make_import("os", flags=[Flag.LAZY], line=2),
        make_import("re", flags=[Flag.LAZY], line=4),
    ]


def test_import_outside_a_function_body_is_not_lazy() -> None:
    """A default value or a decorator is not lazy; the body of a lambda is."""

    # GIVEN dynamic imports in a default value, in a decorator and in a lambda
    source = (
        "@__import__('a').deco\n"
        "def f(x=__import__('b')):\n"
        "    pass\n"
        "g = lambda: __import__('c')\n"
    )

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN only the body of the lambda is lazy
    assert scan.dynamic_imports == [
        make_dynamic("a", call="__import__", line=1),
        make_dynamic("b", call="__import__", line=2),
        make_dynamic("c", call="__import__", line=4, flags=[Flag.LAZY, Flag.DYNAMIC]),
    ]


def test_import_in_a_class_body_is_not_lazy() -> None:
    """An import in a class body is not `lazy`: the body runs at load time."""

    # GIVEN an import in a class body
    source = "class C:\n    import os\n"

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN it has no flag
    assert scan.imports == [make_import("os", line=2)]


def test_import_in_a_type_parameter_or_a_type_statement_is_lazy() -> None:
    """A type parameter and the value of a `type` statement are `lazy`."""

    # GIVEN dynamic imports in the type parameters of a function and of a
    # class, and in the value of a `type` statement, all inside a `try`
    source = (
        "try:\n"
        "    def f[T: __import__('a')](): pass\n"
        "    class C[U: __import__('b')]: pass\n"
        "    type X = __import__('c')\n"
        "except ImportError:\n"
        "    pass\n"
    )

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN they are lazy and not guarded: Python reads them when they are used
    lazy = [Flag.LAZY, Flag.DYNAMIC]
    assert scan.dynamic_imports == [
        make_dynamic("a", call="__import__", line=2, flags=lazy),
        make_dynamic("b", call="__import__", line=3, flags=lazy),
        make_dynamic("c", call="__import__", line=4, flags=lazy),
    ]


def test_generator_expression_is_lazy_but_its_first_iterable() -> None:
    """A generator expression is `lazy`, except its first iterable."""

    # GIVEN a generator expression with imports in the element, in a condition
    # and in both iterables
    source = (
        "g = (__import__('a')\n"
        "     for x in __import__('b')\n"
        "     if __import__('c')\n"
        "     for y in __import__('d'))\n"
    )

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN only the first iterable runs when the generator is created
    lazy = [Flag.LAZY, Flag.DYNAMIC]
    assert sorted(scan.dynamic_imports, key=lambda d: d.line) == [
        make_dynamic("a", call="__import__", line=1, flags=lazy),
        make_dynamic("b", call="__import__", line=2),
        make_dynamic("c", call="__import__", line=3, flags=lazy),
        make_dynamic("d", call="__import__", line=4, flags=lazy),
    ]


def test_import_in_a_try_body_is_guarded() -> None:
    """Only an import in the body of a `try` is `guarded`."""

    # GIVEN imports in the body, the handler, the else and the finally of a try
    source = (
        "try:\n    import a\nexcept ImportError:\n    import b\n"
        "else:\n    import c\nfinally:\n    import d\n"
    )

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN only the import in the body is guarded
    assert scan.imports == [
        make_import("a", flags=[Flag.GUARDED], line=2),
        make_import("b", line=4),
        make_import("c", line=6),
        make_import("d", line=8),
    ]


def test_import_in_a_function_inside_a_try_is_not_guarded() -> None:
    """A `try` around a function or a lambda does not guard its body."""

    # GIVEN imports in a function and in a lambda that are defined in the body
    # of a try
    source = (
        "try:\n"
        "    def f():\n"
        "        import a\n"
        "    g = lambda: __import__('b')\n"
        "except ImportError:\n"
        "    pass\n"
    )

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN the imports are lazy but not guarded
    assert scan.imports == [make_import("a", flags=[Flag.LAZY], line=3)]
    assert scan.dynamic_imports == [
        make_dynamic("b", call="__import__", line=4, flags=[Flag.LAZY, Flag.DYNAMIC])
    ]


def test_import_in_a_try_without_a_handler_is_not_guarded() -> None:
    """A `try` with only `finally` catches nothing, so it guards nothing."""

    # GIVEN an import in the body of a try that has no handler
    source = "try:\n    import a\nfinally:\n    pass\n"

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN it is not guarded
    assert scan.imports == [make_import("a", line=2)]


def test_import_in_a_try_with_except_star_is_guarded() -> None:
    """An import in the body of a `try` with `except*` is `guarded`."""

    # GIVEN an import in the body of a try with except*
    source = "try:\n    import a\nexcept* ImportError:\n    pass\n"

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN it is guarded
    assert scan.imports == [make_import("a", flags=[Flag.GUARDED], line=2)]


def test_star_import_has_no_names() -> None:
    """`from x import *` brings in names that the scan does not know."""

    # GIVEN a star import
    source = "from app.core import *\n"

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN the import has no names
    assert scan.imports == [make_import("app.core", line=1)]


def test_import_in_the_condition_of_an_if_is_found() -> None:
    """A dynamic import in the condition of an `if` is scanned too."""

    # GIVEN a dynamic import in the condition of an if
    source = "if __import__('a'):\n    pass\n"

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN it is found
    assert scan.dynamic_imports == [make_dynamic("a", call="__import__")]


def test_import_under_type_checking_is_type_only() -> None:
    """An import under `TYPE_CHECKING`, bare or an attribute, is `type_only`."""

    # GIVEN imports under TYPE_CHECKING, both the bare name and the attribute,
    # and one in the else branch
    source = (
        "if TYPE_CHECKING:\n    import a\nelse:\n    import b\n"
        "if typing.TYPE_CHECKING:\n    import c\n"
    )

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN the imports under the condition are type-only, the else is not
    assert scan.imports == [
        make_import("a", flags=[Flag.TYPE_ONLY], line=2),
        make_import("b", line=4),
        make_import("c", flags=[Flag.TYPE_ONLY], line=6),
    ]


def test_flags_of_nested_constructs_combine() -> None:
    """An import inside several constructs has the flag of each of them."""

    # GIVEN an import in a try inside a function under TYPE_CHECKING
    source = (
        "if TYPE_CHECKING:\n"
        "    def f():\n"
        "        try:\n"
        "            import a\n"
        "        except ImportError:\n"
        "            pass\n"
    )

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN it has the three flags
    assert scan.imports == [
        make_import("a", flags=[Flag.TYPE_ONLY, Flag.LAZY, Flag.GUARDED], line=4)
    ]


def test_relative_import_starts_from_the_package() -> None:
    """A relative import is resolved from the package of the scanner."""

    # GIVEN relative imports
    source = (
        "from . import render\n"
        "from .render import Renderer\n"
        "from ..core import models\n"
        "from .. import core\n"
    )

    # WHEN it is scanned in the package app.web
    scan = scan_source(source, package="app.web")

    # THEN one dot is the package, two dots its parent, and each import keeps
    # the name as written
    assert scan.imports == [
        make_import("app.web", ["render"], line=1, written="."),
        make_import("app.web.render", ["Renderer"], line=2, written=".render"),
        make_import("app.core", ["models"], line=3, written="..core"),
        make_import("app", ["core"], line=4, written=".."),
    ]


def test_dynamic_import_with_a_literal_has_the_target() -> None:
    """A dynamic import with a literal records the target and the call."""

    # GIVEN the three forms of a dynamic import with a literal, one with the
    # name as a keyword and one in an argument of another call, and a call of
    # `import_module` on another object
    source = (
        "importlib.import_module('a.b')\n"
        "import_module('c')\n"
        "__import__('d')\n"
        "importlib.import_module(name='e')\n"
        "print(__import__('f'))\n"
        "manager.import_module('g')\n"
    )

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN the forms have their target and the name of the call, and the call
    # on another object is not an import
    assert scan.dynamic_imports == [
        make_dynamic("a.b", line=1),
        make_dynamic("c", line=2),
        make_dynamic("d", call="__import__", line=3),
        make_dynamic("e", line=4),
        make_dynamic("f", call="__import__", line=5),
    ]


def test_dynamic_import_without_a_literal_has_no_target() -> None:
    """A dynamic import whose argument is not a literal has no target."""

    # GIVEN dynamic imports whose argument is not a literal string, inside a
    # function
    source = (
        "def f(name):\n"
        "    importlib.import_module(name)\n"
        "    importlib.import_module('a.' + name)\n"
    )

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN they have no target, and the flags of their place
    assert scan.dynamic_imports == [
        make_dynamic(None, line=2, flags=[Flag.LAZY, Flag.DYNAMIC]),
        make_dynamic(None, line=3, flags=[Flag.LAZY, Flag.DYNAMIC]),
    ]


def test_dynamic_import_with_other_arguments_has_no_target() -> None:
    """A call with an argument other than the name has no target."""

    # GIVEN calls with other arguments, which can change the module that the
    # name means
    source = (
        "importlib.import_module('flags', 'app')\n"
        "importlib.import_module(package='app')\n"
        "__import__('flags', globals(), locals(), [], 1)\n"
    )

    # WHEN it is scanned
    scan = scan_source(source)

    # THEN they have no target
    assert scan.dynamic_imports == [
        make_dynamic(None, line=1),
        make_dynamic(None, line=2),
        make_dynamic(None, call="__import__", line=3),
    ]


# The scan of a file on disk


def test_package_of_a_file_is_the_start_of_relative_imports(tmp_path: Path) -> None:
    """A relative import starts from the package of the file."""

    # GIVEN the same relative import in a package and in a module of it
    write_files(
        tmp_path,
        {
            "app/web/__init__.py": "from . import x\n",
            "app/web/views.py": "from . import x\n",
        },
    )

    # WHEN both files are scanned
    package, _ = scan_file(str(tmp_path), "app/web/__init__.py", "app.web")
    module, _ = scan_file(str(tmp_path), "app/web/views.py", "app.web.views")

    # THEN one dot is app.web in both
    assert package.imports == [make_import("app.web", ["x"], written=".")]
    assert module.imports == [make_import("app.web", ["x"], written=".")]


def test_lines_of_a_file_follow_the_parser(tmp_path: Path) -> None:
    """The evidence lines follow the encoding and the line breaks of Python."""

    # GIVEN a Latin-1 file with a lone carriage return, which is a line break
    # for the parser, and a form feed, which is not, before a dynamic import
    source = b"# coding: latin-1\n# caf\xe9\r\x0c\nimport_module('a')\n"
    write_files(tmp_path, {"pkg/__init__.py": ""})
    (tmp_path / "pkg" / "mod.py").write_bytes(source)

    # WHEN the file is scanned
    scan, lines = scan_file(str(tmp_path), "pkg/mod.py", "pkg.mod")

    # THEN the entry of `lines` for the line of the import is its text, and the
    # Latin-1 comment is decoded
    assert lines[scan.dynamic_imports[0].line - 1] == "import_module('a')"
    assert lines[1] == "# caf\u00e9"
