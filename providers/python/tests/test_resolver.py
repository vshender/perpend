"""Tests for `perpend_provider_python.resolver`."""

from pathlib import Path

from perpend_provider_python.resolver import Location
from tests.factories import make_resolver, write_files


def test_resolver_finds_a_package_and_a_module(tmp_path: Path) -> None:
    """A package is its `__init__.py`, another module is its file."""

    # GIVEN a package with a module under a search directory
    write_files(tmp_path, {"src/app/__init__.py": "", "src/app/core.py": ""})
    resolver = make_resolver(tmp_path, ["src"])

    # WHEN the names are resolved
    package = resolver.find("app")
    module = resolver.find("app.core")
    missing = resolver.find("app.nope")

    # THEN the files are relative to the root, modules are looked for under
    # the package only, and a name without a file is not found
    assert package == Location("src/app/__init__.py", [str(tmp_path / "src/app")])
    assert module == Location("src/app/core.py", [])
    assert missing is None


def test_resolver_prefers_a_package_to_a_module(tmp_path: Path) -> None:
    """When a module and a package share a name, the package wins."""

    # GIVEN a module and a package with the same name
    write_files(
        tmp_path,
        {"app/__init__.py": "", "app/dual.py": "", "app/dual/__init__.py": ""},
    )
    resolver = make_resolver(tmp_path)

    # WHEN the name is resolved
    location = resolver.find("app.dual")

    # THEN it is the package
    assert location is not None
    assert location.file == "app/dual/__init__.py"


def test_resolver_takes_the_first_search_directory(tmp_path: Path) -> None:
    """A module under two search directories comes from the first one."""

    # GIVEN the same module under two search directories
    write_files(tmp_path, {"a/mod.py": "", "b/mod.py": ""})
    resolver = make_resolver(tmp_path, ["b", "a"])

    # WHEN the name is resolved
    location = resolver.find("mod")

    # THEN the first directory wins
    assert location is not None
    assert location.file == "b/mod.py"


def test_resolver_looks_for_a_module_only_in_its_package(tmp_path: Path) -> None:
    """A module is not found in a directory that Python does not use."""

    # GIVEN the package pkg under the search directory a, and a directory pkg
    # with the module mod under b; and the module app.utils next to a
    # directory app/utils
    write_files(
        tmp_path,
        {
            "a/pkg/__init__.py": "",
            "b/pkg/mod.py": "",
            "a/app/__init__.py": "",
            "a/app/utils.py": "",
            "a/app/utils/n.py": "",
        },
    )
    resolver = make_resolver(tmp_path, ["a", "b"])

    # WHEN the modules in the other directories are resolved
    in_other_directory = resolver.find("pkg.mod")
    under_a_module = resolver.find("app.utils.n")

    # THEN they are not found: pkg is the package in a, and utils is a module,
    # which has no modules under it
    assert in_other_directory is None
    assert under_a_module is None


def test_directory_without_init_is_a_namespace_package(tmp_path: Path) -> None:
    """A directory without `__init__.py` is a namespace package; at the top
    level only when it is named.
    """

    # GIVEN a directory without `__init__.py` inside a package, and one at
    # the top level under two search directories
    write_files(
        tmp_path,
        {
            "a/app/__init__.py": "",
            "a/app/ns/mod.py": "",
            "a/top/x.py": "",
            "b/top/y.py": "",
        },
    )
    known = make_resolver(tmp_path, ["a", "b"], packages=["top"])
    unknown = make_resolver(tmp_path, ["a", "b"])

    # WHEN the names are resolved
    inside = known.find("app.ns")
    module = known.find("top.y")
    top = known.find("top")
    top_unknown = unknown.find("top")

    # THEN a namespace package has no file, and its directories are all the
    # directories of that name; at the top level it is a package only when
    # the resolver knows its name
    assert inside == Location(None, [str(tmp_path / "a/app/ns")])
    assert module == Location("b/top/y.py", [])
    assert top == Location(None, [str(tmp_path / "a/top"), str(tmp_path / "b/top")])
    assert top_unknown is None


def test_resolver_rejects_a_part_that_is_not_a_file_name(tmp_path: Path) -> None:
    """An empty part or a part with a path separator is not found; a part need
    not be an identifier.
    """

    # GIVEN a package with a subpackage and a file whose name is not an
    # identifier, and files named `.py`, which an empty part would match
    write_files(
        tmp_path,
        {
            "app/__init__.py": "",
            "app/core/__init__.py": "",
            "app/0001_initial.py": "",
            "app/.py": "",
            ".py": "",
        },
    )
    resolver = make_resolver(tmp_path)

    # WHEN a relative name, an empty name, a name with an empty last part, the
    # paths of the subpackage and of the file, and the name of the file are
    # resolved
    relative = resolver.find("..x")
    empty = resolver.find("")
    trailing = resolver.find("app.")
    package_path = resolver.find("app/core")
    file_path = resolver.find("app/0001_initial")
    odd = resolver.find("app.0001_initial")

    # THEN only the name of the file is found
    assert relative is None
    assert empty is None
    assert trailing is None
    assert package_path is None
    assert file_path is None
    assert odd == Location("app/0001_initial.py", [])
