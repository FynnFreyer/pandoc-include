"""
Implements the default transclusion-syntax.

``!include path/to/file.md``
"""
import re

import panflute as pf

from config import parseConfig, Env

# Global variables
INCLUDE_INVALID  = 0
INCLUDE_FILE     = 1
INCLUDE_HEADER   = 2

# Regex patterns
RE_IS_INCLUDE_HEADER  = r"(\\?(!|\$))include-header"
RE_IS_INCLUDE_LINE    = r"^(\\?(!|\$))include(-header)?"
RE_INCLUDE_PATTERN    = r"^(\\?(!|\$))include(-header)?(\`(?P<args>[^\`]+(, ?[^\`]+)*)\`)? ((?P<fname>[^\`\'\"]+)|([\`\'\"])(?P<fnamealt>.+)\9)$"


def extract_info(rawString):
    global options

    includeType = INCLUDE_INVALID
    config = {}
    filename = None

    # wildcards '*' are escaped which needs to be undone because of path globing
    # convert_text has a tendency to produce multiline text which can not be matched correctly
    # Also here we should unescape underscores from markdown_strict.
    rawString = rawString.replace('\\*', '*').replace('\n', ' ').replace('\\_', '_')

    if re.match(RE_IS_INCLUDE_HEADER, rawString):
        includeType = INCLUDE_HEADER
    else:
        includeType = INCLUDE_FILE

    matches = re.match(RE_INCLUDE_PATTERN, rawString)
    if not matches:
        # Pattern was not able to extract args and file glob... Hence, abort
        raise ValueError(f"Unable to extract info from include line {rawString}")

    groups = matches.groupdict()

    # Get filename from Regex capture group
    filename = groups.get('fname', None)
    if not filename:
        filename = groups.get('fnamealt', None)

    # Get args from RegEx capture group
    if 'args' in groups and groups['args']:
        config = parseConfig(groups['args'])

    if not filename:
        raise ValueError(f"Unable to extract info from include line {rawString}")

    return includeType, filename, config


def is_include_line(elem):
    # Revert to Markdown for regex matching
    rawString = pf.convert_text(
        elem,
        input_format='panflute',
        output_format='markdown_strict',
        standalone=True,
        pandoc_path=Env.PandocBin
    )

    includeType = INCLUDE_INVALID
    config = {}
    name = None

    if re.match(RE_IS_INCLUDE_LINE, rawString):
        includeType, name, config = extract_info(rawString)

    return includeType, name, config


def is_code_include(elem):
    try:
        new_elem = pf.convert_text(elem.text, pandoc_path=Env.PandocBin)[0]
    except:
        return INCLUDE_INVALID, None, None

    includeType, name, config = is_include_line(new_elem)
    if includeType == INCLUDE_HEADER:
        pf.debug("[WARN] Invalid !include-header in code blocks")
        includeType = INCLUDE_INVALID

    return includeType, name, config
