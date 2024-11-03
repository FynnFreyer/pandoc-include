"""
Implements Obsidian-style transclusions using image syntax.

``![Transcluded file](path/to/file.md)``
"""

from pandoc_include.syntax import IncludeType

from pathlib import Path
from typing import Optional, Tuple

from panflute import convert_text, CodeBlock, Doc, Image, Para


def is_include_line(elem: Para, doc: Optional[Doc] = None) -> Tuple[IncludeType, Optional[str], Optional[dict]]:
    """
    Determine whether a paragraph is an obsidian-style transclusion.

    :param elem: The paragraph block that might be an include line.
    :param doc: The document we're working on.
    :return: A tuple of ``include_type, file_name, config``.
    """
    include_type, file_name, config = IncludeType.INVALID, None, None
    try:
        [img] = elem.content
        if isinstance(img, Image):
            # TODO this might need to resolve relative to doc location
            path = Path(img.url)
            if path.is_file():
                return IncludeType.FILE, str(path), {}
    except:
        pass

    return include_type, file_name, config


def is_code_include(elem: CodeBlock, doc: Optional[Doc] = None) -> Tuple[IncludeType, Optional[str], Optional[dict]]:
    """
    Determine whether a code block is an obsidian-style transclusion.

    :param elem: The code block that might be an include line.
    :param doc: The document we're working on.
    :return: A tuple of ``include_type, file_name, config``.
    """
    try:
        [cb_content] = convert_text(elem.text)
        if isinstance(cb_content, Para):
            return is_include_line(cb_content)
    except:
        pass

    include_type, file_name, config = IncludeType.INVALID, None, None
    return include_type, file_name, config
