"""Tests for `perpend_provider_python.provider`."""

from pathlib import Path

from perpend_provider_python.records import (
    Artifact,
    Evidence,
    Fact,
    Flag,
    Unresolved,
)
from tests.factories import scan_repository

CALL = "importlib.import_module(...)"
"""The target of an unresolved record that names a call of `import_module`."""

# The records of an import statement


def test_names_that_are_submodules_are_facts_of_their_own(tmp_path: Path) -> None:
    """A name that is a submodule gives a fact with no names, not a name."""

    # GIVEN an import, in a function, of two names from app.core, where models
    # is a module and helper is not
    text = "from app.core import models, helper"
    files = {
        "app/__init__.py": "",
        "app/core/__init__.py": "",
        "app/core/models.py": "",
        "app/main.py": f"def f():\n    {text}\n",
    }

    # WHEN the provider scans the repository
    records = scan_repository(tmp_path, files)

    # THEN the fact about the package has the name helper, the fact about
    # models.py has no names, and both facts have the flag of the statement
    where = Evidence(2, text)
    assert records == [
        Fact(
            "app/main.py",
            Artifact("file", "app/core/__init__.py"),
            ["helper"],
            [Flag.LAZY],
            where,
        ),
        Fact(
            "app/main.py",
            Artifact("file", "app/core/models.py"),
            [],
            [Flag.LAZY],
            where,
        ),
    ]


def test_import_of_a_module_is_a_fact_about_its_file(tmp_path: Path) -> None:
    """`import a.b` is one fact about the file of `a.b`, with no names."""

    # GIVEN an import of the module app.core.models
    text = "import app.core.models"
    files = {
        "app/__init__.py": "",
        "app/core/__init__.py": "",
        "app/core/models.py": "",
        "app/main.py": f"{text}\n",
    }

    # WHEN the provider scans the repository
    records = scan_repository(tmp_path, files)

    # THEN there is one fact about the file of the module
    assert records == [
        Fact(
            "app/main.py",
            Artifact("file", "app/core/models.py"),
            [],
            [],
            Evidence(1, text),
        )
    ]


def test_external_package_keeps_the_names(tmp_path: Path) -> None:
    """An import from a submodule of an external package keeps its names."""

    # GIVEN an import from requests.adapters, where requests is not in the
    # repository
    text = "from requests.adapters import HTTPAdapter"
    files = {"app/__init__.py": "", "app/main.py": f"{text}\n"}

    # WHEN the provider scans the repository
    records = scan_repository(tmp_path, files)

    # THEN the fact is about the top-level package, with the name
    assert records == [
        Fact(
            "app/main.py",
            Artifact("package", "requests"),
            ["HTTPAdapter"],
            [],
            Evidence(1, text),
        )
    ]


def test_missing_module_of_a_repository_package_is_unresolved(tmp_path: Path) -> None:
    """`import app.missing` is unresolved when app is in the repository."""

    # GIVEN an absolute and a relative import of app.missing, where app has no
    # module missing
    absolute = "import app.missing"
    relative = "from .missing import x"
    files = {"app/__init__.py": "", "app/main.py": f"{absolute}\n{relative}\n"}

    # WHEN the provider scans the repository
    records = scan_repository(tmp_path, files)

    # THEN each record is unresolved, with the module as written
    assert records == [
        Unresolved("app/main.py", "app.missing", Evidence(1, absolute)),
        Unresolved("app/main.py", ".missing", Evidence(2, relative)),
    ]


def test_import_of_a_directory_without_init_gives_no_record(tmp_path: Path) -> None:
    """An import of a directory without `__init__.py` gives no record; a name
    that it lacks is unresolved.
    """

    # GIVEN the directory app/ns without `__init__.py`, with a module in it,
    # and three imports: of the directory by its name, of the directory as a
    # name of app, and of two names from the directory
    text = "from app.ns import mod, missing"
    files = {
        "app/__init__.py": "",
        "app/ns/mod.py": "",
        "app/main.py": f"import app.ns\nfrom app import ns\n{text}\n",
    }

    # WHEN the provider scans the repository
    records = scan_repository(tmp_path, files)

    # THEN the imports of the directory give no record, the module in it is a
    # fact, and a name that the directory does not have is unresolved
    where = Evidence(3, text)
    assert records == [
        Fact("app/main.py", Artifact("file", "app/ns/mod.py"), [], [], where),
        Unresolved("app/main.py", "app.ns", where),
    ]


# The record of a dynamic import


def test_dynamic_import_of_a_repository_file_is_a_fact(tmp_path: Path) -> None:
    """A literal that names a repository module gives a `dynamic` fact."""

    # GIVEN a call whose literal names a module of the repository
    text = "import_module('app.plugin')"
    files = {"app/__init__.py": "", "app/plugin.py": "", "app/main.py": f"{text}\n"}

    # WHEN the provider scans the repository
    records = scan_repository(tmp_path, files)

    # THEN the record is a fact about the file, with the dynamic flag
    assert records == [
        Fact(
            "app/main.py",
            Artifact("file", "app/plugin.py"),
            [],
            [Flag.DYNAMIC],
            Evidence(1, text),
        )
    ]


def test_dynamic_import_of_an_external_module_is_a_package_fact(tmp_path: Path) -> None:
    """A literal that names an external module gives a package fact."""

    # GIVEN a call whose literal names a module outside the repository
    text = "import_module('json.decoder')"
    files = {"app/__init__.py": "", "app/main.py": f"{text}\n"}

    # WHEN the provider scans the repository
    records = scan_repository(tmp_path, files)

    # THEN the record is a fact about the top-level package
    assert records == [
        Fact(
            "app/main.py",
            Artifact("package", "json"),
            [],
            [Flag.DYNAMIC],
            Evidence(1, text),
        )
    ]


def test_dynamic_import_of_a_missing_submodule_is_unresolved(tmp_path: Path) -> None:
    """A literal that names a missing repository module is unresolved."""

    # GIVEN a call whose literal names a submodule that a package of the
    # repository does not have
    text = "import_module('app.missing')"
    files = {"app/__init__.py": "", "app/main.py": f"{text}\n"}

    # WHEN the provider scans the repository
    records = scan_repository(tmp_path, files)

    # THEN the record is unresolved, with the name as written
    assert records == [Unresolved("app/main.py", "app.missing", Evidence(1, text))]


def test_dynamic_import_of_text_that_is_not_a_name_is_unresolved(
    tmp_path: Path,
) -> None:
    """A literal that is not a module name gives no package fact."""

    # GIVEN a call whose literal is a space
    text = "import_module(' ')"
    files = {"app/__init__.py": "", "app/main.py": f"{text}\n"}

    # WHEN the provider scans the repository
    records = scan_repository(tmp_path, files)

    # THEN the record is unresolved, with the call as the target
    assert records == [Unresolved("app/main.py", CALL, Evidence(1, text))]


def test_dynamic_import_without_a_usable_literal_is_unresolved(tmp_path: Path) -> None:
    """A dynamic import without a usable literal is unresolved."""

    # GIVEN calls without a literal, with an empty one and with a relative one
    texts = ["import_module(name)", "import_module('')", "import_module('.x')"]
    files = {"app/__init__.py": "", "app/main.py": "\n".join(texts) + "\n"}

    # WHEN the provider scans the repository
    records = scan_repository(tmp_path, files)

    # THEN all are unresolved, with the call as the target
    assert records == [
        Unresolved("app/main.py", CALL, Evidence(line, text))
        for line, text in enumerate(texts, start=1)
    ]
