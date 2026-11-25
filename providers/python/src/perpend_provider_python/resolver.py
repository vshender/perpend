"""The lookup of module names on the file system."""

import os
from collections.abc import Collection
from dataclasses import dataclass
from typing import final


@dataclass(frozen=True)
class Location:
    """Where a module is on the file system."""

    file: str | None
    """The file of the module, relative to the repository root, with `/` as the
    separator:

    - the `__init__.py` file of a package;
    - the `.py` file of a module that is not a package;
    - `None` for a namespace package: a directory without `__init__.py`,
      which Python imports as a package that has no file of its own.
    """

    directories: list[str]
    """The directories where the modules under this one are looked for: what
    Python keeps as the `__path__` of the module.  They are absolute, because
    they are used to look for files and are never printed:

    - a package with `__init__.py` has one, its own directory;
    - a namespace package has one or more: every directory of that name in
      the directories where the name is looked for, which Python joins into
      one package;
    - a module that is not a package has none, so no name is found under it.
    """


@final
class Resolver:
    """A finder of modules by their names, on the file system only: it does not
    look at `sys.path` or at installed packages.

    A dotted name is resolved part by part, as Python imports it: the first part
    is looked for in the search directories, and every next part only in the
    directories of the part before it.  So `a.b` is never found in a directory
    `a` that Python would not use for the package `a`.  For example, with the
    search directories `one` and `two`, in this order, and the files
    `one/a/__init__.py` and `two/a/b.py`, the name `a.b` is not found: `a` is
    the package in `one`, and Python does not look for `b` in `two`.

    A part is compared with the names in a directory exactly, with their case.
    On a file system that ignores case, a check of the path alone would find
    `app/models.py` for the name `app.Models`.  The import system of Python also
    compares a part of a module name exactly.  The exception: on some platforms,
    such as Windows or macOS, it compares without case when the environment
    variable `PYTHONCASEOK` is set.  The resolver ignores `PYTHONCASEOK`.

    The entries of a directory are read once and kept, because one run looks
    up the same names many times.  So the resolver sees a directory as it was
    at that first reading: a file that appears later is not found.

    One part is looked for in a list of directories in two steps:

    - the first directory that has the package `part/__init__.py` or the module
      `part.py` wins; when a directory has both, the package wins;
    - otherwise the directories named `part` that have no `__init__.py`, taken
      together, are a namespace package.  For the first part of a name, this
      step applies only to the packages that the resolver was given, see
      `packages`.
    """

    def __init__(self, root: str, search: list[str], packages: Collection[str]) -> None:
        """Create a resolver.

        Args:
            root: The repository root, an absolute path.
            search: The absolute search directories, in priority order.  Each is
                the root or a directory under it, because the files that the
                resolver finds are given relative to the root.
            packages: The names of the top-level packages that the caller knows
                to be in the repository.  A directory without `__init__.py` that
                is right under a search directory is a package only when its
                name is one of them.  The reason: the resolver cannot see
                installed packages.  If every such directory counted, a
                directory `docker` with scripts would be taken for the package
                `docker`, and `import docker` would no longer be reported as an
                import of the installed package.
        """

        self.root: str = root
        self.search: list[str] = search
        self.packages: Collection[str] = packages
        self._cache: dict[str, Location | None] = {}
        self._entries_cache: dict[str, set[str]] = {}

    def find(self, name: str) -> Location | None:
        """Return the location of the module `name`, or `None`.

        The result is `None` when the module is not found under the search
        directories.  Two kinds of parts are never found:

        - an empty part: a file named `.py` is not a module;
        - a part that holds a path separator: no entry of a directory has such
          a name.

        Such parts can come from the literal of a call such as
        `importlib.import_module("app.")` or `importlib.import_module("a/b")`.
        """

        if name not in self._cache:
            self._cache[name] = self._find(name)

        return self._cache[name]

    def _find(self, name: str) -> Location | None:
        """Resolve the module `name` part by part."""

        location = None
        directories = self.search
        for index, part in enumerate(name.split(".")):
            if not part:
                return None

            allow_namespace = index > 0 or part in self.packages
            location = self._find_part(directories, part, allow_namespace)
            if location is None:
                return None
            directories = location.directories

        return location

    def _find_part(
        self,
        directories: list[str],
        part: str,
        allow_namespace: bool,
    ) -> Location | None:
        """Return the location of the module `part` in `directories`.

        Args:
            directories: The absolute directories to look in, in priority order.
            part: One part of a dotted module name.
            allow_namespace: Whether a directory without `__init__.py` counts as
                a namespace package.
        """

        for directory in directories:
            package = os.path.join(directory, part)
            init = os.path.join(package, "__init__.py")
            module = package + ".py"
            if (
                self._has_entry(directory, part)
                and self._has_entry(package, "__init__.py")
                and os.path.isfile(init)
            ):
                return Location(self._relative(init), [package])
            elif self._has_entry(directory, part + ".py") and os.path.isfile(module):
                return Location(self._relative(module), [])

        if allow_namespace:
            found = [
                os.path.join(directory, part)
                for directory in directories
                if self._has_entry(directory, part)
                and os.path.isdir(os.path.join(directory, part))
            ]
            if found:
                return Location(None, found)

        return None

    def _has_entry(self, directory: str, name: str) -> bool:
        """Return whether `directory` has an entry named exactly `name`.

        A directory that cannot be read has no entries: the import system of
        Python does the same for a directory that is missing, that is not a
        directory, or that it cannot read.
        """

        entries = self._entries_cache.get(directory)
        if entries is None:
            try:
                entries = set(os.listdir(directory))
            except OSError:
                entries = set[str]()
            self._entries_cache[directory] = entries

        return name in entries

    def _relative(self, file: str) -> str:
        """Return the absolute path `file` relative to the root, with `/`."""

        return os.path.relpath(file, self.root).replace(os.sep, "/")
