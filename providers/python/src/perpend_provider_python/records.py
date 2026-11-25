"""The records of the output: the interface between the provider and Perpend.

The output is in the protocol `perpend.facts/1`.  The Perpend repository
defines the protocol in `lib/facts/facts.mli`.

Every path in the output is relative to the repository root, with `/` as the
separator.
"""

import json
from dataclasses import asdict, dataclass, field
from enum import StrEnum
from typing import Literal

from perpend_provider_python import __version__

PROVIDER_NAME = "python"
"""The name of the provider in the header."""

PROTOCOL = "perpend.facts/1"
"""The protocol of the output, named in the header."""

CAPABILITIES = (
    "file_imports",
    "imported_names",
    "package_imports",
    "unresolved",
    "context_flags",
    "file_inventory",
)
"""What the provider is able to report, listed in the header.  The protocol
says what each name promises.
"""


class Flag(StrEnum):
    """A property of an import besides its target: for example, when it runs or
    how it is written.  One import can have several flags: for example, a call
    to `importlib.import_module` in the body of a function is both `lazy` and
    `dynamic`.

    A fact prints its flags in the order of this class, so that the same flags
    always print the same way.
    """

    TYPE_ONLY = "type_only"
    """The import is for the type checker only: it is in the body of an `if`
    whose condition is the name `TYPE_CHECKING` or an attribute with that name,
    such as `if typing.TYPE_CHECKING:`.
    """

    LAZY = "lazy"
    """The import is in code that runs only when something uses it: for example,
    in the body of a function or a `lambda`, or in the expression before the
    first `for` in a generator expression.  So the module may load without the
    import.  The provider does not follow calls: the code can still run during
    the load, such as the body of a function that the module calls at the top
    level.
    """

    GUARDED = "guarded"
    """The import is in the body of a `try` that has a handler, so the code may
    catch a failed import.  Code in that body that runs later, such as the body
    of a function defined there, is not guarded: it can run after the `try` has
    ended, and the provider does not follow where it is called.  Any handler
    counts, whatever exception class it names: a class that the project defines
    can be a subclass of `ImportError`, and the provider does not look at base
    classes.
    """

    DYNAMIC = "dynamic"
    """The import is a call, such as `importlib.import_module`, not a statement.
    """


@dataclass(frozen=True)
class Header:
    """The header: the first record, which names the provider."""

    type: Literal["provider"] = field(default="provider", init=False)
    """The type of the record, as the protocol names it."""

    protocol: str = field(default=PROTOCOL, init=False)
    """The protocol of the output."""

    name: str = field(default=PROVIDER_NAME, init=False)
    """The name of the provider."""

    version: str = field(default=__version__, init=False)
    """The version of the provider."""

    languages: tuple[str, ...] = field(default=("python",), init=False)
    """The languages that the provider covers."""

    capabilities: tuple[str, ...] = field(default=CAPABILITIES, init=False)
    """What the provider is able to report."""


@dataclass(frozen=True)
class File:
    """A file that the provider covers: it read the file and reported its
    imports.

    The record is printed also for a file without imports, so that such a file
    is in the output too, as the capability `file_inventory` promises.
    """

    type: Literal["file"] = field(default="file", init=False)
    """The type of the record, as the protocol names it."""

    path: str
    """The file, relative to the root."""

    language: str
    """The language of the file."""


@dataclass(frozen=True)
class Artifact:
    """What a fact points to: a repository file or an external package."""

    kind: Literal["file", "package"]
    """The kind of the artifact: a repository file or an external package."""

    id: str
    """The name of the artifact, in a form that depends on `kind`:

    - for a file, its path;
    - for a package, the top-level name of the imported module, such as
      `requests` for `import requests.adapters`.  It is the name that Python
      imports, which can differ from the name of the distribution: `yaml` from
      `PyYAML`.
    """


@dataclass(frozen=True)
class Evidence:
    """Where an import is written.  Evidence is for people, so that they can
    open the file at the right line; it is never part of the identity of a fact,
    since an edit can move it.
    """

    line: int
    """The line where the import starts, from 1."""

    text: str
    """The text of that line, without the white space around it."""


@dataclass(frozen=True)
class Fact:
    """A dependency: the file `subject` needs the artifact `object`.

    The names are printed once each, sorted, and the flags in the order of
    `Flag`, so that the same dependency always prints the same way.
    """

    type: Literal["fact"] = field(default="fact", init=False)
    """The type of the record, as the protocol names it."""

    subject: str
    """The file with the import, relative to the root."""

    object: Artifact
    """What it depends on."""

    precision: Literal["syntactic"] = field(default="syntactic", init=False)
    """How `object` was found.  `syntactic` means that the provider read the
    import and looked the module up by its name under the search directories
    (the `--path` options), without running Python.  So `object` can be wrong
    when the project finds its modules in another way, for example when its
    code changes `sys.path`.  The protocol defines other precisions too.
    """

    names: list[str]
    """The names that the import takes from the imported module, as `test` in
    `from app.helpers import test`.  A name that the provider finds as a
    submodule in the repository is not here, because the import then depends
    on the submodule, not on a name of the module:

    - `from app import helpers`, with the file `app/helpers.py`, gives a fact
      about that file with no names;
    - `from app import ns`, with a directory `app/ns` that has no
      `__init__.py`, gives no record: there is no file to depend on.

    Empty when the import brings in a whole module, as `import app`, or when
    the names are not known, as for `from m import *`.
    """

    flags: list[Flag]
    """The context of the import."""

    evidence: Evidence
    """Where the import is written."""

    def __post_init__(self) -> None:
        """Put the names and the flags in their printed order."""

        object.__setattr__(self, "names", sorted(set(self.names)))
        object.__setattr__(self, "flags", [f for f in Flag if f in self.flags])


@dataclass(frozen=True)
class Unresolved:
    """An import that the provider cannot follow to a file or to an external
    package, such as `import app.missing` when `app` is a package of the
    repository and has no module `missing`.  The record keeps the import
    visible: without it, an import that the provider cannot follow would look
    like no dependency at all.
    """

    type: Literal["unresolved"] = field(default="unresolved", init=False)
    """The type of the record, as the protocol names it."""

    subject: str
    """The file with the import, relative to the root."""

    target: str
    """The module as the import statement or the literal of a dynamic call
    writes it, such as `..core` for `from ..core import x`.  When the argument
    of a dynamic call is not a module name that the provider can look up, the
    target names the call instead; `call_target` builds that form.
    """

    evidence: Evidence
    """Where it is written."""


type Record = Header | File | Fact | Unresolved
"""A record of the output: one line of it."""


def file_artifact(path: str) -> Artifact:
    """Return the artifact of the repository file `path`."""

    return Artifact("file", path)


def package_artifact(name: str) -> Artifact:
    """Return the artifact of the external package of the module `name`."""

    return Artifact("package", name.split(".")[0])


def call_target(call: str) -> str:
    """Return the target of an unresolved record for the dynamic call `call`:
    the full name of the function with `(...)`, so that a reader sees a call
    and not a module name.  For example, `importlib.import_module(...)`.
    """

    return f"{call}(...)"


def to_json(record: Record) -> str:
    """Return `record` as one line of JSON."""

    return json.dumps(asdict(record), separators=(",", ":"))
