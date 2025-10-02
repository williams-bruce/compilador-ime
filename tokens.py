# Defines the structure of a token and enumerates all possible token types

from enum import Enum, auto
from dataclasses import dataclass
from typing import Optional

class TokenType(Enum):
    """Enumeration for all token types."""

    # Palavras reservadas
    ARRAY = 0
    BOOLEAN = auto()
    BREAK = auto()
    CHAR = auto()
    CONTINUE = auto()
    DO = auto()
    ELSE = auto()
    FALSE = auto()
    FUNCTION = auto()
    IF = auto()
    INTEGER = auto()
    OF = auto()
    STRING = auto()
    STRUCT = auto()
    TRUE = auto()
    TYPE = auto()
    VAR = auto()
    WHILE = auto()

    # Simbolos
    COLON = auto()          # :
    PLUS = auto()           # +
    MINUS = auto()          # -
    SEMI_COLON = auto()     # ;
    COMMA = auto()          # ,
    LEFT_SQUARE = auto()    # [
    RIGHT_SQUARE = auto()   # ]
    LEFT_BRACES = auto()    # {
    RIGHT_BRACES = auto()   # }
    LEFT_PARENTHESIS = auto() # (
    RIGHT_PARENTHESIS = auto() # )
    TIMES = auto()          # *
    DOT = auto()            # .
    DIVIDE = auto()         # /
    EQUALS = auto()         # =
    LESS_THAN = auto()      # <
    GREATER_THAN = auto()   # >
    NOT = auto()            # !

    # Operadores
    PLUS_PLUS = auto()      # ++
    MINUS_MINUS = auto()    # --
    EQUAL_EQUAL = auto()    # ==
    AND = auto()            # &&
    OR = auto()             # ||
    LESS_OR_EQUAL = auto()  # <=
    GREATER_OR_EQUAL = auto() # >=
    NOT_EQUAL = auto()      # !=

    # Tokens regulares
    CHARACTER = auto()
    NUMERAL = auto()
    STRINGVAL = auto()
    ID = auto()

    # Tokens desconhecidos
    EOF = auto()
    UNKNOWN = auto()


@dataclass
class Token:
    """Represents a token with its type, value, line, and column."""
    type: TokenType
    lexeme: str
    line: int
    col: int
    secondary_token: Optional[int] = None

    def __str__(self):
        """String representation for easy debugging."""
        sec_token_str = f", secondary: {self.secondary_token}" if self.secondary_token is not None else ""
        return f"Token({self.type.name}, '{self.lexeme}', line {self.line}, col {self.col}{sec_token_str})"


# Mapping of keywords to their token types
KEYWORDS = {
    "array": TokenType.ARRAY,
    "boolean": TokenType.BOOLEAN,
    "break": TokenType.BREAK,
    "char": TokenType.CHAR,
    "continue": TokenType.CONTINUE,
    "do": TokenType.DO,
    "else": TokenType.ELSE,
    "false": TokenType.FALSE,
    "function": TokenType.FUNCTION,
    "if": TokenType.IF,
    "integer": TokenType.INTEGER,
    "of": TokenType.OF,
    "string": TokenType.STRING, # Also a literal type, but "string" is a keyword for type declaration
    "struct": TokenType.STRUCT,
    "true": TokenType.TRUE,
    "type": TokenType.TYPE,
    "var": TokenType.VAR,
    "while": TokenType.WHILE,
}