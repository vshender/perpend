"""Tests for `perpend_provider_python.cli`: the whole provider as a command."""

from pathlib import Path

from tests.factories import FIXTURE, run_provider, write_files


def test_facts_of_the_fixture_match_the_golden_file() -> None:
    """The provider prints the golden facts of the fixture project."""

    # GIVEN the fixture project and its golden facts
    golden = (FIXTURE / "facts.jsonl").read_text()

    # WHEN the provider runs on the package app
    result = run_provider("--path", "src", "--package", "app")

    # THEN it prints the golden facts, a summary on stderr, and exits with 0
    assert result.returncode == 0, result.stderr
    assert result.stdout == golden
    assert result.stderr == "9 files, 19 facts\n"


def test_root_and_path_default_to_the_current_directory() -> None:
    """Without `--root` and `--path`, the current directory is both."""

    # GIVEN the provider run from the directory of the packages

    # WHEN it runs without --root and --path
    result = run_provider("--package", "app", cwd=FIXTURE / "src")

    # THEN the paths are relative to that directory
    assert result.returncode == 0, result.stderr
    assert (
        '{"type":"file","path":"app/__init__.py","language":"python"}' in result.stdout
    )


def test_relative_import_above_the_top_level_is_unresolved(tmp_path: Path) -> None:
    """A relative import above the top-level package is an unresolved record."""

    # GIVEN the module app.web.render with an import of app.core, which is one
    # level up, and an import two levels up, which is above the package app
    write_files(
        tmp_path,
        {
            "app/__init__.py": "",
            "app/core.py": "",
            "app/web/__init__.py": "",
            "app/web/render.py": "from ..core import f\nfrom ...x import y\n",
        },
    )

    # WHEN the provider runs on the package
    result = run_provider("--package", "app", cwd=tmp_path)

    # THEN the first import is a fact about app/core.py, and the second is
    # unresolved, with the name as written
    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines()[5:] == [
        '{"type":"fact","subject":"app/web/render.py",'
        '"object":{"kind":"file","id":"app/core.py"},"precision":"syntactic",'
        '"names":["f"],"flags":[],'
        '"evidence":{"line":1,"text":"from ..core import f"}}',
        '{"type":"unresolved","subject":"app/web/render.py","target":"...x",'
        '"evidence":{"line":2,"text":"from ...x import y"}}',
    ]


def test_every_python_file_under_a_package_is_covered(tmp_path: Path) -> None:
    """A file is covered whatever its name and the name of its directory, unless
    it is hidden or in `__pycache__`.
    """

    # GIVEN a package with a module in a directory without `__init__.py`, a
    # file and a directory whose names are not identifiers, a hidden file, and
    # files in a hidden directory and in `__pycache__`
    write_files(
        tmp_path,
        {
            "app/__init__.py": "from app.ns import mod\n",
            "app/ns/mod.py": "",
            "app/migrations/0001_initial.py": "",
            "app/my-dir/tool.py": "",
            "app/.hidden.py": "",
            "app/.hidden/skip.py": "",
            "app/__pycache__/skip.py": "",
        },
    )

    # WHEN the provider runs on the package
    result = run_provider("--package", "app", cwd=tmp_path)

    # THEN every file is covered but the hidden ones and the cache, and the
    # import finds the module in the directory without `__init__.py`
    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines()[1:] == [
        '{"type":"file","path":"app/__init__.py","language":"python"}',
        '{"type":"file","path":"app/migrations/0001_initial.py","language":"python"}',
        '{"type":"file","path":"app/my-dir/tool.py","language":"python"}',
        '{"type":"file","path":"app/ns/mod.py","language":"python"}',
        '{"type":"fact","subject":"app/__init__.py",'
        '"object":{"kind":"file","id":"app/ns/mod.py"},"precision":"syntactic",'
        '"names":[],"flags":[],'
        '"evidence":{"line":1,"text":"from app.ns import mod"}}',
    ]


def test_namespace_package_is_made_of_its_directories(tmp_path: Path) -> None:
    """A package without `__init__.py` is scanned in every search directory."""

    # GIVEN the package ns without `__init__.py` under two search directories
    write_files(tmp_path, {"one/ns/a.py": "from ns import b\n", "two/ns/b.py": ""})

    # WHEN the provider runs on the package with both directories
    result = run_provider(
        "--path", "one", "--path", "two", "--package", "ns", cwd=tmp_path
    )

    # THEN the files of both directories are covered, and a module of one
    # directory imports a module of the other
    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines()[1:] == [
        '{"type":"file","path":"one/ns/a.py","language":"python"}',
        '{"type":"file","path":"two/ns/b.py","language":"python"}',
        '{"type":"fact","subject":"one/ns/a.py",'
        '"object":{"kind":"file","id":"two/ns/b.py"},"precision":"syntactic",'
        '"names":[],"flags":[],"evidence":{"line":1,"text":"from ns import b"}}',
    ]


def test_syntax_error_stops_the_provider() -> None:
    """A file that does not parse stops the provider with an error."""

    # GIVEN a package with a file that does not parse

    # WHEN the provider runs on it
    result = run_provider("--path", "src", "--package", "broken")

    # THEN it prints nothing on stdout, names the file and the line on stderr,
    # and exits with 1
    assert result.returncode == 1
    assert result.stdout == ""
    assert result.stderr == (
        "perpend-provider-python: src/broken/bad.py, line 1: syntax error: "
        "def broken(:\n"
    )


def test_file_under_two_module_names_is_an_error() -> None:
    """A file that two packages share is an error."""

    # GIVEN the search directories src and src/app, under which app/core is
    # both app.core and core

    # WHEN the provider runs on both packages
    result = run_provider(
        "--path", "src", "--path", "src/app", "--package", "app", "--package", "core"
    )

    # THEN it exits with 1, names the file and the modules, and says what to do
    assert result.returncode == 1
    assert result.stdout == ""
    assert result.stderr == (
        "perpend-provider-python: src/app/core/__init__.py is both the module "
        "'app.core' and 'core'; name only one of the packages 'app' and 'core'\n"
    )


def test_search_directory_outside_the_root_is_an_error() -> None:
    """A `--path` directory outside the root is an error."""

    # GIVEN a search directory above the root

    # WHEN the provider runs with it
    result = run_provider("--path", "..", "--package", "app")

    # THEN it exits with 1 and names the directory
    assert result.returncode == 1
    assert result.stdout == ""
    assert result.stderr == (
        "perpend-provider-python: search directory '..' is outside the root\n"
    )


def test_unknown_package_is_an_error() -> None:
    """A package that is not under the search directories is an error."""

    # GIVEN a package that is not under the search directories

    # WHEN the provider runs on it
    result = run_provider("--path", "src", "--package", "nope")

    # THEN it exits with 1 and names the package
    assert result.returncode == 1
    assert result.stdout == ""
    assert result.stderr.startswith(
        "perpend-provider-python: package 'nope' not found under the search "
        "directories: "
    )


def test_module_instead_of_a_package_is_an_error(tmp_path: Path) -> None:
    """A single file is not a package, so it cannot be scanned."""

    # GIVEN a module that is one file, not a directory with `__init__.py`
    write_files(tmp_path, {"mod.py": ""})

    # WHEN the provider runs on it
    result = run_provider("--package", "mod", cwd=tmp_path)

    # THEN it exits with 1 and says that the name is a module, not a package
    assert result.returncode == 1
    assert result.stderr == (
        "perpend-provider-python: 'mod' is a module, not a package: mod.py\n"
    )


def test_name_that_is_not_a_top_level_package_is_an_error() -> None:
    """A package name must be one identifier: no dot and no path."""

    # GIVEN a dotted name, and a name with a slash, which the completion of a
    # path in a shell gives

    # WHEN the provider runs on each of them
    dotted = run_provider("--path", "src", "--package", "app.core")
    slash = run_provider("--path", "src", "--package", "app/")

    # THEN both exit with 1 and say that the name is not a top-level package
    assert dotted.returncode == 1
    assert dotted.stderr == (
        "perpend-provider-python: 'app.core' is not a top-level package\n"
    )
    assert slash.returncode == 1
    assert (
        slash.stderr == "perpend-provider-python: 'app/' is not a top-level package\n"
    )


def test_file_that_cannot_be_read_is_an_error(tmp_path: Path) -> None:
    """A file that cannot be read stops the provider with an error."""

    # GIVEN a package with a file that is a link to nothing
    write_files(tmp_path, {"app/__init__.py": ""})
    (tmp_path / "app" / "gone.py").symlink_to(tmp_path / "missing.py")

    # WHEN the provider runs on the package
    result = run_provider("--package", "app", cwd=tmp_path)

    # THEN it exits with 1 and names the file
    assert result.returncode == 1
    assert result.stdout == ""
    assert result.stderr == (
        "perpend-provider-python: app/gone.py: No such file or directory\n"
    )


def test_code_too_deep_to_parse_is_an_error(tmp_path: Path) -> None:
    """Code that is nested too deeply to parse is an error, not a crash."""

    # GIVEN a package with an expression of 200000 unary minus signs
    write_files(tmp_path, {"app/__init__.py": "x = " + "-" * 200000 + "1\n"})

    # WHEN the provider runs on the package
    result = run_provider("--package", "app", cwd=tmp_path)

    # THEN it exits with 1 and names the file
    assert result.returncode == 1
    assert result.stdout == ""
    assert result.stderr == (
        "perpend-provider-python: app/__init__.py: the code is nested too deeply\n"
    )


def test_directory_that_cannot_be_read_is_an_error(tmp_path: Path) -> None:
    """A directory that cannot be read stops the provider with an error."""

    # GIVEN a package with a directory that nobody may read
    write_files(tmp_path, {"app/__init__.py": "", "app/closed/mod.py": ""})
    (tmp_path / "app" / "closed").chmod(0)

    # WHEN the provider runs on the package
    try:
        result = run_provider("--package", "app", cwd=tmp_path)
    finally:
        (tmp_path / "app" / "closed").chmod(0o755)

    # THEN it exits with 1 and names the directory, relative to the root
    assert result.returncode == 1
    assert result.stdout == ""
    assert result.stderr == ("perpend-provider-python: app/closed: Permission denied\n")
