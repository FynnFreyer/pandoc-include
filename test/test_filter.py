import sys
from pprint import pp
from pathlib import Path

from util import process_file


def test_main():
    _test_dir = Path(__file__).parent
    doc = process_file(_test_dir / "test.md")
    pp(doc, stream=sys.stderr)


def test_BA():
    doc_path = Path("/home/fynn/Documents/Notizen/Uni/Bachelorarbeit/doc/de/thesis.md")
    doc = process_file(doc_path)
    pp(doc, stream=sys.stderr)


if __name__ == '__main__':
    test_BA()
