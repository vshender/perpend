"""Tests for `perpend_provider_python.records`."""

from perpend_provider_python.records import Fact, Flag, package_artifact
from tests.factories import WHERE


def test_fact_prints_its_names_and_flags_in_one_order() -> None:
    """A fact has each name once, sorted, and its flags in the `Flag` order."""

    # GIVEN names that repeat and are not sorted, and flags in another order
    names = ["b", "a", "b"]
    flags = [Flag.GUARDED, Flag.TYPE_ONLY]

    # WHEN a fact is built from them
    fact = Fact("app/__init__.py", package_artifact("x"), names, flags, WHERE)

    # THEN the names are unique and sorted, and the flags follow `Flag`
    assert fact.names == ["a", "b"]
    assert fact.flags == [Flag.TYPE_ONLY, Flag.GUARDED]
