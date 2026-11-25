"""Perpend facts provider for Python code.

The provider finds the imports of one or more packages, which the `--package`
options name, and prints them for Perpend, a change-control layer for software
architecture.  The output follows the Perpend protocol `perpend.facts/1`: one
JSON object per line on stdout.

The provider reads the code and never runs it.  A pass over the syntax tree of
every file gives the imports with their names, their flags (see `records.Flag`)
and their lines.  The file of an imported module is found by its name, part by
part, under the search directories (the `--path` options).
"""

__version__ = "0.1"
"""The version of the provider."""
