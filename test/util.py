import sys
import inspect

from io import StringIO
from pathlib import Path
from pprint import pp

from panflute import convert_text, Doc, load

from pandoc_include.main import main


def show(caller_location: str = "", die: bool = False):
    """Show locals at call site, and maybe die."""
    print(f"{caller_location}\n", file=sys.stderr)
    frame = inspect.currentframe()
    try:
        locs = frame.f_back.f_locals
        pp(locs, stream=sys.stderr)
    finally:
        del frame

    if die:
        print("AAAARRRRGGGHHH\n\n\n", file=sys.stderr)
        sys.exit(0)


def process_file(path: Path) -> Doc:
    """
    Filter a file with the main method.

    :param path: The file to filter.
    :return: The resulting document.
    """
    with path.open() as file:
        dump = convert_text(file.read(), output_format="json")

    with StringIO(dump) as input, StringIO() as output:
        main(input_stream=input, output_stream=output)
        json_result = output.getvalue()

    with StringIO(json_result) as result:
        return load(result)
