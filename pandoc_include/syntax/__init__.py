"""
`Namespace-package <https://packaging.python.org/en/latest/guides/packaging-namespace-packages/>`_
that implements common syntax variants for document transclusion.

Users can extend this on their own, and pass the chosen syntax variant via document meta-data.
"""

from enum import IntEnum


class IncludeType(IntEnum):
    INVALID = 0
    """Invalid include."""

    FILE = 1
    """File-include."""

    HEADER = 2
    """Header-include."""
