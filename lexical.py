# Uses a list of regular expressions to identify tokens in the source code

import re
import sys
from typing import Dict, List, Generator
from tokens import Token, TokenType, KEYWORDS


class SymbolTable:
    """Manages identifiers and constants to avoid duplication."""
    def __init__(self):
        self.identifiers: Dict[str, int] = {}
        self._next_id_index: int = 0
        self.constants: List[str] = []
        self._const_map: Dict[str, int] = {}


    def lookup_or_add_identifier(self, name: str) -> int:
        """Adds an identifier if new, returns its unique index."""
        if name not in self.identifiers:
            self.identifiers[name] = self._next_id_index
            self._next_id_index += 1
        return self.identifiers[name]


    def add_constant(self, value: str) -> int:
        """Adds a constant literal if new, returns its unique index."""
        if value not in self._const_map:
            self._const_map[value] = len(self.constants)
            self.constants.append(value)
        return self._const_map[value]


    def get_constant(self, index: int) -> str:
        """Returns the constant value for a given index."""
        return self.constants[index]


    def __str__(self) -> str:
        """Provides a formatted string of the symbol table's contents."""
        report = []
        if self.identifiers:
            report.append("Identifiers:")
            sorted_ids = sorted(self.identifiers.items(), key=lambda item: item[1])
            for name, index in sorted_ids:
                report.append(f"  {index}: {name}")
        else:
            report.append("Identifiers: None")

        report.append("\n")

        if self.constants:
            report.append("Constants:")
            for i, const in enumerate(self.constants):
                report.append(f"  {i}: {const}")
        else:
            report.append("Constants: None")

        return "\n".join(report)


class Lexer:
    """
    A regex-based lexical analyzer that transforms source code into a stream of tokens.
    """
    # Order matters: '==' must come before '='
    TOKEN_SPECIFICATION = [
        # --- Multi-character operators ---
        ('PLUS_PLUS',       r'\+\+'),
        ('MINUS_MINUS',     r'--'),
        ('EQUAL_EQUAL',     r'=='),
        ('NOT_EQUAL',       r'!='),
        ('LESS_OR_EQUAL',   r'<='),
        ('GREATER_OR_EQUAL',r'>='),
        ('AND',             r'&&'),
        ('OR',              r'\|\|'),

        # --- Literals and Identifiers ---
        ('STRINGVAL',       r'"(?:\\.|[^"\\])*"'),  # Handles escaped quotes
        ('CHARACTER',       r"'(.)'"),
        ('NUMERAL',         r'\d+'),
        ('ID',              r'[a-zA-Z_][a-zA-Z0-9_]*'),

        # --- Single-character symbols ---
        ('PLUS',            r'\+'),
        ('MINUS',           r'-'),
        ('TIMES',           r'\*'),
        ('DIVIDE',          r'/'),
        ('EQUALS',          r'='),
        ('LESS_THAN',       r'<'),
        ('GREATER_THAN',    r'>'),
        ('LEFT_PARENTHESIS',r'\('),
        ('RIGHT_PARENTHESIS',r'\)'),
        ('LEFT_SQUARE',     r'\['),
        ('RIGHT_SQUARE',    r'\]'),
        ('LEFT_BRACES',     r'{'),
        ('RIGHT_BRACES',    r'}'),
        ('COMMA',           r','),
        ('DOT',             r'\.'),
        ('SEMI_COLON',      r';'),
        ('COLON',           r':'),
        ('NOT',             r'!'),

        # --- Utility patterns ---
        ('NEWLINE',         r'\n'),
        ('SKIP',            r'[ \t\r]+'),
        ('UNKNOWN',         r'.'),
    ]

    # Master regex to match all token patterns
    TOKEN_REGEX = re.compile('|'.join(f'(?P<{name}>{pattern})' for name, pattern in TOKEN_SPECIFICATION))


    def __init__(self, source_code: str):
        self.source_code = source_code
        self.symbol_table = SymbolTable()
        self.line_num = 1
        self.line_start_pos = 0


    def tokenize(self) -> Generator[Token, None, None]:
        """Yields tokens from the source code."""
        pos = 0
        while pos < len(self.source_code):
            match = self.TOKEN_REGEX.match(self.source_code, pos)
            if not match:
                # This should not happen with the UNKNOWN pattern
                raise RuntimeError(f"Unexpected error at position {pos}")

            token_type_name = match.lastgroup
            lexeme = match.group()
            col = match.start() - self.line_start_pos + 1
            pos = match.end()

            if token_type_name == 'NEWLINE':
                self.line_num += 1
                self.line_start_pos = pos
                continue
            elif token_type_name == 'SKIP':
                continue
            
            token_type = TokenType[token_type_name]
            secondary_token: int|None = None

            if token_type == TokenType.ID:
                # Check if the identifier is a keyword
                token_type = KEYWORDS.get(lexeme, TokenType.ID)
                if token_type == TokenType.ID:
                    secondary_token = self.symbol_table.lookup_or_add_identifier(lexeme)

            elif token_type in (TokenType.NUMERAL, TokenType.STRINGVAL, TokenType.CHARACTER):
                # For string and char, store the value without quotes
                value_to_store = lexeme
                if token_type in (TokenType.STRINGVAL, TokenType.CHARACTER):
                    value_to_store = lexeme[1:-1]
                secondary_token = self.symbol_table.add_constant(value_to_store)

            yield Token(token_type, lexeme, self.line_num, col, secondary_token)
        
        # Yield the End-Of-File token
        yield Token(TokenType.EOF, '', self.line_num, len(self.source_code) - self.line_start_pos + 1)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python lexical.py <filename>")
        sys.exit(1)

    filename = sys.argv[1]
    try:
        with open(filename, "r") as file:
            source = file.read()
            lexer = Lexer(source)
            has_errors = False
            
            print("--- Tokens ---")
            for token in lexer.tokenize():
                print(token)
                if token.type == TokenType.UNKNOWN:
                    print(f"Error: Unrecognized character '{token.lexeme}' at line {token.line}, column {token.col}", file=sys.stderr)
                    has_errors = True
            
            print("\n--- Symbol Table ---")
            print(lexer.symbol_table)

            if not has_errors:
                print("\nCompiled successfully.")
            else:
                print("\nCompilation failed due to lexical errors.")

    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"An unexpected error occurred: {e}", file=sys.stderr)
        sys.exit(1)