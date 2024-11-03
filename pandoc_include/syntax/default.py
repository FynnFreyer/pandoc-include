"""
Implements the default transclusion-syntax.

``!include path/to/file.md``
"""
import re

import panflute as pf

from config import parseConfig, Env
from pandoc_include.syntax import IncludeType

# Regex patterns
RE_IS_INCLUDE_HEADER = r"(\\?(!|\$))include-header"
RE_IS_INCLUDE_LINE   = r"^(\\?(!|\$))include(-header)?"
RE_INCLUDE_PATTERN   = r"^(\\?(!|\$))include(-header)?(\`(?P<args>[^\`]+(, ?[^\`]+)*)\`)? ((?P<fname>[^\`\'\"]+)|([\`\'\"])(?P<fnamealt>.+)\9)$"


def extract_info(raw_string):
    config = {}

    # wildcards '*' are escaped which needs to be undone because of path globing
    # convert_text has a tendency to produce multiline text which can not be matched correctly
    # Also here we should unescape underscores from markdown_strict.
    raw_string = raw_string.replace('\\*', '*').replace('\n', ' ').replace('\\_', '_')

    if re.match(RE_IS_INCLUDE_HEADER, raw_string):
        include_type = IncludeType.HEADER
    else:
        include_type = IncludeType.FILE

    matches = re.match(RE_INCLUDE_PATTERN, raw_string)
    if not matches:
        # Pattern was not able to extract args and file glob... Hence, abort
        raise ValueError(f"Unable to extract info from include line {raw_string}")

    groups = matches.groupdict()

    # Get filename from Regex capture group
    filename = groups.get('fname', None)
    if not filename:
        filename = groups.get('fnamealt', None)

    # Get args from RegEx capture group
    if 'args' in groups and groups['args']:
        config = parseConfig(groups['args'])

    if not filename:
        raise ValueError(f"Unable to extract info from include line {raw_string}")

    return include_type, filename, config


def is_include_line(elem):
    # Revert to Markdown for regex matching
    raw_string = pf.convert_text(
        elem,
        input_format='panflute',
        output_format='markdown_strict',
        standalone=True,
        pandoc_path=Env.PandocBin
    )

    include_type = IncludeType.INVALID
    config = {}
    name = None

    if re.match(RE_IS_INCLUDE_LINE, raw_string):
        include_type, name, config = extract_info(raw_string)

    return include_type, name, config


def is_code_include(elem):
    try:
        new_elem = pf.convert_text(elem.text, pandoc_path=Env.PandocBin)[0]
    except:
        return IncludeType.INVALID, None, None

    include_type, name, config = is_include_line(new_elem)
    if include_type == IncludeType.HEADER:
        pf.debug("[WARN] Invalid !include-header in code blocks")
        include_type = IncludeType.INVALID

    return include_type, name, config
