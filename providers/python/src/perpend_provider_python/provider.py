"""The provider: the files of the packages and the records of their imports."""

import os
from typing import final

from perpend_provider_python.errors import ProviderError
from perpend_provider_python.records import (
    Evidence,
    Fact,
    File,
    Header,
    Record,
    Unresolved,
    call_target,
    file_artifact,
    package_artifact,
)
from perpend_provider_python.resolver import Resolver
from perpend_provider_python.scanner import DynamicImport, Import, scan_file


def _is_utf_8(text: str) -> bool:
    r"""Return whether `text` can be written as UTF-8.

    Python can hold text that has no UTF-8 form: a file name with bytes that
    are not UTF-8, which Python decodes with surrogate escapes, or a string
    literal such as `"\udc80"`.  The JSON output writes such text as an
    escape, and the facts reader of Perpend, in `lib/facts`, rejects it: it
    requires every string to be valid UTF-8 after the escapes are decoded.
    """

    try:
        text.encode("utf-8")
    except UnicodeEncodeError:
        return False
    return True


def _sort_key(record: Fact | Unresolved) -> tuple[int, str, str]:
    """Return the key that sorts the records that come from one line of a file.

    - A fact sorts by the kind and the id of its object.  Kinds sort by name,
      so files come before packages.
    - An unresolved record sorts by its target, after every fact.
    """

    if isinstance(record, Fact):
        return (0, record.object.kind, record.object.id)
    else:
        return (1, "", record.target)


@final
class Provider:
    """The builder of the facts of the packages under one repository.

    The provider covers the `.py` files under the directories of the packages.
    It leaves out `__pycache__`, hidden files and hidden directories, whose
    names start with a dot, and it does not follow symbolic links to
    directories inside them.  It scans each file and looks up the imported
    modules with a `Resolver`.
    """

    files: dict[str, str]
    """The covered files, relative to the root, with the names of their modules.
    """

    facts: list[Fact]
    """The facts, in the order of the output."""

    def __init__(self, root: str, search: list[str], packages: list[str]) -> None:
        """Create a provider.

        Args:
            root: The repository root, an absolute path.
            search: The absolute search directories, in priority order.  Each is
                the root or a directory under it.
            packages: The top-level packages to scan.
        """

        self.root: str = root
        self.packages: list[str] = packages
        self._resolver = Resolver(root, search, packages)
        self._unresolved: list[Unresolved] = []
        self.files = {}
        self.facts = []

    def run(self) -> None:
        """Find the files, the facts and the unresolved records.

        Raises:
            ProviderError: The packages cannot be scanned; the message says why.
        """

        for package in self.packages:
            for directory in self._package_directories(package):
                self._find_files(directory, package)
        for path in sorted(self.files):
            self._scan_file(path)

    def _package_directories(self, package: str) -> list[str]:
        """Return the absolute directories of the top-level `package`.

        A package with `__init__.py` has one directory.  A package without
        `__init__.py` is a namespace package: it is made of every directory with
        its name right under a search directory.

        Raises:
            ProviderError: The name is not a single identifier, names a module
                file and not a package, or is not found.
        """

        if not package.isidentifier():
            raise ProviderError(f"'{package}' is not a top-level package")

        location = self._resolver.find(package)
        if location is None:
            raise ProviderError(
                f"package '{package}' not found under the search directories: "
                + ", ".join(self._resolver.search)
            )
        if not location.directories:
            raise ProviderError(
                f"'{package}' is a module, not a package: {location.file}"
            )

        return location.directories

    def _find_files(self, directory: str, package: str) -> None:
        """Find every `.py` file under `directory`, a directory of `package`,
        and add it to `files` with the name of its module.

        A directory is covered even without `__init__.py`, and even when its
        name is not an identifier, such as `my-dir`.  No import statement can
        name a file under `my-dir`, but the file is still code with imports of
        its own.  Hidden directories and hidden files, whose names start with a
        dot, and `__pycache__` are left out.  Symbolic links to directories
        under `directory` are not followed.

        Raises:
            ProviderError: A directory cannot be read, a file name is not valid
                UTF-8, or a file is already covered as another module.  The last
                can happen when a search directory is the directory of a named
                package or is inside it, and a package from that search
                directory is named too:

                - Another package: with the search directories `.` and
                  `app/vendor` and the packages `app` and `lib`, the file
                  `app/vendor/lib/__init__.py` is both the module
                  `app.vendor.lib` and the module `lib`.
                - The same package, when it has no `__init__.py` and so is made
                  of every directory with its name under a search directory:
                  with the search directories `.` and `ns/inner` and the
                  package `ns`, the file `ns/inner/ns/x.py` is both the module
                  `ns.inner.ns.x` and the module `ns.x`.
        """

        def fail(error: OSError) -> None:
            """Stop on a directory that cannot be read."""

            path = os.path.relpath(error.filename, self.root).replace(os.sep, "/")
            raise ProviderError(f"{path}: {error.strerror}")

        for current, directories, names in os.walk(directory, onerror=fail):
            directories[:] = sorted(
                name
                for name in directories
                if not name.startswith(".") and name != "__pycache__"
            )
            relative = os.path.relpath(current, directory)
            parts = [package] if relative == "." else [package, *relative.split(os.sep)]
            for name in sorted(names):
                if name.startswith(".") or not name.endswith(".py"):
                    continue

                stem = name.removesuffix(".py")
                module = ".".join(parts if stem == "__init__" else [*parts, stem])
                path = os.path.relpath(os.path.join(current, name), self.root)
                path = path.replace(os.sep, "/")
                if not _is_utf_8(path):
                    raise ProviderError(f"{path!r}: the file name is not valid UTF-8")

                known = self.files.get(path)
                if known is not None and known != module:
                    # Two named packages share the file, or one package reaches
                    # it through two of its directories.  Only the first case
                    # has a simple fix to suggest.  In the second case the user
                    # has to change the search directories.
                    first = known.split(".")[0]
                    hint = "the search directories overlap"
                    if first != package:
                        hint = (
                            "name only one of the packages "
                            + f"'{first}' and '{package}'"
                        )
                    raise ProviderError(
                        f"{path} is both the module '{known}' and '{module}'; {hint}"
                    )

                self.files[path] = module

    def _scan_file(self, path: str) -> None:
        """Scan the file `path` and add its facts and its unresolved records to
        the provider.

        The facts of the file come in the order of its lines, and so do its
        unresolved records.  The records of one line are ordered by `_sort_key`.
        """

        scan, lines = scan_file(self.root, path, self.files[path])

        def where(line: int) -> Evidence:
            """Return the evidence for `line` of the file."""

            return Evidence(line, lines[line - 1].strip())

        found: list[tuple[int, Fact | Unresolved]] = []
        for statement in scan.imports:
            for record in self._import_records(path, statement, where(statement.line)):
                found.append((statement.line, record))
        for above in scan.imports_above_top_level:
            record = Unresolved(path, above.written, where(above.line))
            found.append((above.line, record))
        for dynamic in scan.dynamic_imports:
            dynamic_record = self._dynamic_record(path, dynamic, where(dynamic.line))
            if dynamic_record is not None:
                found.append((dynamic.line, dynamic_record))

        found.sort(key=lambda item: (item[0], _sort_key(item[1])))
        for _, record in found:
            if isinstance(record, Fact):
                self.facts.append(record)
            else:
                self._unresolved.append(record)

    def _in_repository(self, module: str) -> bool:
        """Return whether the resolver finds the top-level name of `module`.

        When it does, the name belongs to the repository.  So a module under
        that name that the resolver does not find, such as `app.missing` when
        `app` is found, is missing from the repository and is not an external
        package.
        """

        return self._resolver.find(module.split(".")[0]) is not None

    def _import_records(
        self,
        subject: str,
        statement: Import,
        where: Evidence,
    ) -> list[Fact | Unresolved]:
        """Return the records for one imported module of an import statement
        in the file `subject`.

        In `from module import name1, name2`, the names are what stands after
        `import`.  Each name is looked up as a module under `module`, such as
        `module.name1`:

        - a name that has a file is a submodule: it gives a fact about that
          file.  The list of names of this fact is empty, because the import
          takes the whole submodule, not a name from it;
        - a name that is a directory without `__init__.py` gives no record: the
          directory has no file to depend on.  An import gives a fact only
          about the file of the module that it names, not about the
          `__init__.py` files of the packages above it;
        - every other name is taken as a name that `module` defines.  The
          provider does not check that `module` really defines it.

        When every name of a `from` import is found as a module under `module`,
        so that there are no other names, nothing more is recorded.  Otherwise,
        when there are other names, or for `import module`, which has no names,
        `module` itself gets at most one record:

        - a fact about its file, with the other names, when `module` has a file;
        - when `module` is a directory without `__init__.py`: an unresolved
          record if there are other names, because such a directory defines no
          names, and no record for `import module`, because the directory has no
          file to depend on;
        - an unresolved record when `module` is not found but the resolver finds
          its top-level name: the module is missing;
        - a fact about an external package, with the other names, when the
          top-level name is not found either.

        `from module import *` gives the same record as `import module`: the
        names that it brings in are not known.

        The target of an unresolved record is the module as the statement writes
        it.

        Args:
            subject: The file with the import, relative to the root.
            statement: The imported module, with the names and the flags of its
                statement.
            where: The evidence, shared by the records.
        """

        module = statement.module
        records: list[Fact | Unresolved] = []
        rest: list[str] = []
        for name in statement.names:
            submodule = self._resolver.find(f"{module}.{name}")
            if submodule is None:
                rest.append(name)
            elif submodule.file is not None:
                obj = file_artifact(submodule.file)
                records.append(Fact(subject, obj, [], statement.flags, where))

        if statement.names and not rest:
            return records

        location = self._resolver.find(module)
        if location is None:
            if self._in_repository(module):
                records.append(Unresolved(subject, statement.written, where))
            else:
                obj = package_artifact(module)
                records.append(Fact(subject, obj, rest, statement.flags, where))
        elif location.file is not None:
            obj = file_artifact(location.file)
            records.append(Fact(subject, obj, rest, statement.flags, where))
        elif rest:
            records.append(Unresolved(subject, statement.written, where))

        return records

    def _dynamic_record(
        self,
        subject: str,
        dynamic: DynamicImport,
        where: Evidence,
    ) -> Fact | Unresolved | None:
        """Return the record of a dynamic import in the file `subject`.

        The record depends on the module name in the scanned call, such as
        `"app.x"` in `importlib.import_module("app.x")`.  The name is in
        `dynamic.target` when it is a string literal and the only argument of
        the call.  The cases:

        - no literal, an empty or relative literal, or text that has no UTF-8
          form: an unresolved record whose target names the call: the full
          name of the function with `(...)`, such as
          `importlib.import_module(...)`.  Python rejects an empty name, and a
          relative name in a call that has no other argument;
        - a module that has a file: a fact about the file;
        - a directory without `__init__.py`: `None`, as for an import statement;
        - a module that is not found, under a top-level name that the resolver
          finds: an unresolved record whose target is the literal;
        - any other literal whose first part is an identifier: a fact about
          the external package that this part names;
        - any other literal: an unresolved record whose target names the call.
          No import statement can name a package whose name is not an
          identifier, so the literal is probably not a module name.

        Every fact gets the flags of the call.

        Args:
            subject: The file with the call, relative to the root.
            dynamic: The dynamic import.
            where: The evidence of the record.
        """

        target = dynamic.target
        flags = dynamic.flags
        if not target or target.startswith(".") or not _is_utf_8(target):
            return Unresolved(subject, call_target(dynamic.call), where)

        location = self._resolver.find(target)
        if location is None:
            if self._in_repository(target):
                return Unresolved(subject, target, where)
            if not target.split(".")[0].isidentifier():
                return Unresolved(subject, call_target(dynamic.call), where)
            return Fact(subject, package_artifact(target), [], flags, where)
        if location.file is None:
            return None

        return Fact(subject, file_artifact(location.file), [], flags, where)

    def records(self) -> list[Record]:
        """Return every record of the output.

        The header comes first, as the protocol requires.  The file records
        follow, sorted by path, then the facts and then the unresolved records,
        each in the order of the files and of their lines.  So the same
        repository always gives the same output.
        """

        files = [File(path, "python") for path in sorted(self.files)]
        return [Header(), *files, *self.facts, *self._unresolved]
