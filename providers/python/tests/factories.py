"""Factories of the test data, shared by the tests."""

import ast
import subprocess
import sys
from pathlib import Path

from perpend_provider_python.provider import Provider
from perpend_provider_python.records import Evidence, Fact, Flag, Unresolved
from perpend_provider_python.resolver import Resolver
from perpend_provider_python.scanner import DynamicImport, Import, Scan, Scanner

FIXTURE = Path(__file__).parent / "fixture"
"""The fixture project: a small repository, with the facts that the provider
must print for it in `facts.jsonl`.
"""

WHERE = Evidence(1, "import x")
"""Evidence for the records of the tests that do not look at it."""


def scan_source(source: str, package: str = "app") -> Scan:
    """Return the scan of the Python `source` of a module of `package`."""

    scanner = Scanner(package)
    scanner.visit(ast.parse(source))
    return scanner.scan


def make_import(
    module: str,
    names: list[str] | None = None,
    flags: list[Flag] | None = None,
    line: int = 1,
    written: str | None = None,
) -> Import:
    """Return an import of `module`; no names and no flags by default.

    The module is written as its absolute name unless `written` is given.
    """

    return Import(line, module, written or module, names or [], flags or [])


def make_dynamic(
    target: str | None,
    call: str = "importlib.import_module",
    line: int = 1,
    flags: list[Flag] | None = None,
) -> DynamicImport:
    """Return a dynamic import of `target`; only `Flag.DYNAMIC` by default."""

    return DynamicImport(line, call, target, flags or [Flag.DYNAMIC])


def write_files(root: Path, files: dict[str, str]) -> None:
    """Write `files` under `root`: relative paths with their contents."""

    for file, content in files.items():
        path = root / file
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)


def make_resolver(
    root: Path,
    search: list[str] | None = None,
    packages: list[str] | None = None,
) -> Resolver:
    """Return a resolver for the repository at `root`.

    The search directories `search` are relative to the root; the root itself
    by default.  No package is known to be in the repository by default.
    """

    directories = [str(root / directory) for directory in search or ["."]]
    return Resolver(str(root), directories, packages or [])


def scan_repository(root: Path, files: dict[str, str]) -> list[Fact | Unresolved]:
    """Return the facts and the unresolved records of the package `app` in a
    repository at `root` that has `files`.

    The files, relative paths with their contents, are written to disk.  The
    root is the search directory.  The records come in the order of the output.
    """

    write_files(root, files)
    provider = Provider(str(root), [str(root)], ["app"])
    provider.run()
    return [r for r in provider.records() if isinstance(r, Fact | Unresolved)]


def run_provider(*args: str, cwd: Path = FIXTURE) -> subprocess.CompletedProcess[str]:
    """Run the provider as a separate process with the arguments `args`."""

    return subprocess.run(
        [sys.executable, "-m", "perpend_provider_python", *args],
        cwd=cwd,
        capture_output=True,
        text=True,
    )
