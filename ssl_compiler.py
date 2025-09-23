from lexical import Lexer
from syntatical import SyntaticalAnalyzer
import sys

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python ssl_compiler.py <filename>")
        sys.exit(1)

    filename = sys.argv[1]
    with open(filename, "r") as file:
        source = file.read()
        lexer = Lexer(source)
        syntatical = SyntaticalAnalyzer(lexer)
        syntatical.analyze()