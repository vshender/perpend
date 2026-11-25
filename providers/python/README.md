# Perpend facts provider for Python

`perpend-provider-python` finds the imports of Python packages and prints them
as Perpend facts, protocol `perpend.facts/1`, on stdout.  It reads the code and
never runs it: a pass over the syntax tree of every file gives the imports with
their names, their flags and their lines.

The facts are the input of Perpend, a change-control layer for software
architecture.  Perpend shows the dependencies between the parts of a project,
and the manifest of the project, `perpend.yaml`, describes those parts.  A fact
says that one file depends on another file or on an external package, and where
the import is written.

The Perpend repository defines the protocol in `lib/facts/facts.mli`.  The
output has one JSON object per line:

- a header;
- a record per covered file (see Use);
- a fact per dependency;
- unresolved records: imports that the provider cannot follow to a file or to an
  external package, such as `import app.missing` when `app` is a package of the
  repository and has no module `missing`, or a relative import above the
  top-level package.

The records and the flags of this provider are described in
`src/perpend_provider_python/records.py`.

## Install

From the root of the Perpend repository:

```
uv tool install ./providers/python
```

or `pipx install ./providers/python`.

The provider needs Python 3.14 or newer.  It parses the code with the grammar of
the Python that runs it, so a file that this grammar does not accept, such as
Python 2 code, stops the provider (see Errors).

## Use

From the root of the repository to scan, name every top-level package and the
directory where the packages are:

```
perpend-provider-python --path src --package myapp > facts.jsonl
```

The facts in `facts.jsonl` are the input of Perpend.  For example,
`perpend graph` prints the dependencies between the modules that the manifest of
the repository, `perpend.yaml`, describes:

```
perpend graph --facts facts.jsonl
```

The options of the provider:

- `--package NAME` names a top-level package to scan.  Repeat it for several
  packages.  It is required.  A package without `__init__.py` is a namespace
  package: it is made of every directory with its name right under a `--path`
  directory.
- `--path DIR` names a directory where the packages are found, relative to the
  root, such as `src`.  It must be the root or a directory under it, because the
  paths in the output are relative to the root.  Repeat it for several
  directories.  By default the packages are looked for in the root.
- `--root DIR` names the repository root.  The paths in the output are relative
  to it.  By default it is the current directory.

The provider covers the `.py` files under the directories of the named packages:
it lists each file in the output and reports its imports.  A directory needs no
`__init__.py`, and a name such as `0001_initial.py` or `my-dir` is fine: no
import statement can name such a file, but its own imports are still
dependencies.  Hidden files, hidden directories and `__pycache__` are left out.

The file of an imported module is found by its name, part by part.  The first
part is looked for in the `--path` directories, and every next part only in the
directories of the package that the part before it names.  When a package with
`__init__.py` or a module file is under several `--path` directories, the first
of them in the order of the options wins.  The provider does not search
`sys.path`, the search path of its own Python.  When the provider is installed
in a virtual environment under the root, such as the `.venv` of the project,
`sys.path` holds the `site-packages` directory of that environment, and the
packages installed there would look like repository files.

Two cases follow from this lookup:

- A package with `__init__.py` or a module file that is right under a `--path`
  directory but is not named with `--package` is not scanned, but an import of
  it still gives a fact about its file.  For example, with
  `--path src --package app`, the statement `from helpers import text` in a
  file of `app` gives a fact about `src/helpers/text.py` when
  `src/helpers/__init__.py` exists.
- An import of a directory without `__init__.py` gives no fact about that
  directory: Python imports it as a package that has no file to depend on.
  At the top level such a directory is a package only when `--package` names
  it: otherwise its name is taken as an external package.  See Namespace
  packages.

A dynamic import is recognised by the names in the call only:
`importlib.import_module(...)`, a call of the bare name `import_module(...)`,
and `__import__(...)`.  The provider does not check that the name
`import_module` comes from `importlib`.  A call whose only argument is a string
literal is resolved as an import statement is, and its fact gets the flag
`dynamic`.  It gives an unresolved record instead when the literal is empty, is
relative or has no UTF-8 form, or when it names no module of the repository and
its first part is not an identifier.  A call with any other argument gives an
unresolved record too.

## Namespace packages

A directory without `__init__.py` is a namespace package: Python imports it as a
package that has no file of its own, and joins every directory with that name on
`sys.path` into one package.  The provider follows the same rule over the
`--path` directories.  Because such a package has no file:

- an import of the package itself, such as `import a.b` or `from a import b`,
  gives no record: there is no file to depend on, and an import gives no fact
  about the packages above it (see Limits);
- a name taken from it, as in `from a.b import name`, gives an unresolved record
  unless `name` is a module in it: the package defines no names;
- a top-level one is a package only when it is named with `--package`.
  Otherwise its name is taken as an external package, so that a directory such
  as `docker` does not hide the installed package `docker`.

## Limits

The provider reads the text of the code, so the dependencies that it reports can
differ from what Python imports when the code runs.  The known cases:

- An import gives a fact about the file of the module that it names, not about
  the `__init__.py` files of the packages above it, which Python also runs.  So
  `import a.b`, where `a.b` is a directory without `__init__.py` inside a
  package and there is no module file `a/b.py`, gives no record at all.
- When `pkg/__init__.py` defines `name` and the file `pkg/name.py` exists too,
  `from pkg import name` takes the name from `__init__.py` in Python, but the
  provider reports the file `pkg/name.py`.
- `from pkg import name` gives a fact about `pkg/__init__.py` with `name` in
  its names even when `pkg` defines no such name and Python fails on the
  import.  The provider does not read what a module defines.
- An alias of `importlib`, of `import_module` or of `TYPE_CHECKING`, such as
  `from importlib import import_module as load`, is not recognised: a call
  through the alias gives no record, and an import under the alias of
  `TYPE_CHECKING` does not get the flag `type_only`.
- A symbolic link to a directory inside a named package is not followed when the
  files are looked for, so the files behind it are not covered.  But an import
  of a module behind the link still gives a fact about the file of that module.
- Only `.py` files are looked for.  When an extension module (`.so`, `.pyd`) is
  next to a `.py` file of the same name, or a module of the repository has the
  name of a module that is built into Python, such as `sys`, Python imports the
  extension or the built-in one, and the provider reports the `.py` file.
- A package that is split over several directories with `pkgutil.extend_path` is
  covered in its first `--path` directory only.  For a module in another of its
  directories, `import pkg.x` gives an unresolved record, and
  `from pkg import x` gives a fact about `pkg/__init__.py` in the first `--path`
  directory, with `x` in its names.
- A change of `sys.path` when the code runs is not seen: modules are looked for
  only in the `--path` directories.
- The flag `guarded` does not depend on the class of the handler: an import in
  the body of a `try` with only `except KeyError` gets it too, although that
  handler cannot catch a failed import.
- An import in code that runs later, such as the body of a function or a
  generator expression, is not `guarded` by a `try` around that code, even when
  the code runs before the `try` ends: for example, when the function is called
  in the same `try`, or the generator expression is passed to `tuple`.  The
  provider does not follow where such code runs.
- An import in code that runs later is `lazy` even when the module runs that
  code while it loads: for example, when the import is in a function that the
  module calls at its top level.  The provider does not follow calls.
- An annotation outside a function body is not `lazy`, although Python 3.14
  evaluates such annotations only when they are read: the provider does not know
  which Python runs the code.
- On some platforms, such as Windows or macOS, Python matches module names
  without case when the environment variable `PYTHONCASEOK` is set.  The
  provider always matches them with case.

## Errors

The provider stops with exit code 1 and a message on stderr when:

- a `--package` name is not a single identifier (`a.b` is not allowed), names a
  module file and not a package, or is not found;
- a `--path` directory is outside the root;
- a file is reached under two module names, which can happen when a `--path`
  directory is the directory of a named package or is inside it, and a package
  from that `--path` directory is named too.  For example, with
  `--path . --path app/vendor --package app --package lib`, the file
  `app/vendor/lib/__init__.py` is both the module `app.vendor.lib` and the
  module `lib`;
- a file or a directory of a named package cannot be read;
- a file name is not valid UTF-8;
- a file does not parse, or its code is nested too deeply.

A wrong command line, such as one without `--package`, stops it with exit code
2 and a usage message on stderr.

On success, one line with the numbers of files and facts goes to stderr.

## Develop

From the root of the Perpend repository:

```
cd providers/python
uv sync
make check
```
