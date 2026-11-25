"""The command line of the provider."""

import argparse
import os
import sys

from perpend_provider_python.errors import ProviderError
from perpend_provider_python.provider import Provider
from perpend_provider_python.records import to_json


def parse_args(argv: list[str] | None) -> argparse.Namespace:
    """Parse the command line `argv`, or the process arguments if `None`."""

    parser = argparse.ArgumentParser(
        prog="perpend-provider-python",
        description="Print the imports of Python packages as Perpend facts.",
    )
    parser.add_argument(
        "--root",
        default=".",
        help="the repository root; the paths in the output are relative to it "
        "(default: the current directory)",
    )
    parser.add_argument(
        "--package",
        action="append",
        required=True,
        help="a top-level package to scan; can be repeated",
    )
    parser.add_argument(
        "--path",
        action="append",
        default=[],
        help="a directory where packages are found, relative to the root; "
        "can be repeated (default: the root)",
    )

    return parser.parse_args(argv)


def search_directory(root: str, path: str) -> str:
    """Return the absolute search directory for the `--path` value `path`.

    Args:
        root: The repository root, an absolute path.
        path: The value of `--path`: a directory, relative to the root or
            absolute.

    Raises:
        ProviderError: The directory is outside the root `root`.  The paths of
            the output are relative to the root, so a file outside it cannot be
            reported.
    """

    directory = os.path.abspath(os.path.join(root, path))
    if os.path.commonpath([root, directory]) != root:
        raise ProviderError(f"search directory '{path}' is outside the root")

    return directory


def main(argv: list[str] | None = None) -> int:
    """Run the provider and print the facts to stdout.

    On success, one line with the numbers of files and facts goes to stderr.

    The modules are looked for in the `--path` directories only, not in
    `sys.path`, the search path of the Python that runs the provider.  When the
    provider is installed in a virtual environment under the root, `sys.path`
    holds the `site-packages` directory of that environment, and an installed
    package such as `requests` would be reported as a file of the repository,
    not as an external package.

    Returns:
        The exit code: 0 on success, 1 on an error of the provider, which goes
        to stderr.  A wrong command line ends the process with code 2, as
        `argparse` does.
    """

    args = parse_args(argv)
    root = os.path.abspath(args.root)
    try:
        search = [search_directory(root, path) for path in args.path or ["."]]
        provider = Provider(root, search, args.package)
        provider.run()
    except ProviderError as error:
        print(f"perpend-provider-python: {error}", file=sys.stderr)
        return 1

    for record in provider.records():
        sys.stdout.write(to_json(record) + "\n")

    print(f"{len(provider.files)} files, {len(provider.facts)} facts", file=sys.stderr)
    return 0
