import sys
from types import SimpleNamespace
from ply import yacc
from ply.lex import LexToken

# Our existing lexer and token definitions
from tokens import TokenType
from lexical import Lexer


# -------------------------------
# Token list expected by PLY Yacc
# -------------------------------
# These names must be strings matching the token names used in the grammar rules below.
tokens = (
    'ARRAY', 'BOOLEAN', 'BREAK', 'CHAR', 'CONTINUE', 'DO', 'ELSE', 'FALSE',
    'FUNCTION', 'IF', 'INTEGER', 'OF', 'STRING', 'STRUCT', 'TRUE', 'TYPE',
    'VAR', 'WHILE',
    'COLON', 'PLUS', 'MINUS', 'SEMI_COLON', 'COMMA',
    'LEFT_SQUARE', 'RIGHT_SQUARE', 'LEFT_BRACES', 'RIGHT_BRACES',
    'LEFT_PARENTHESIS', 'RIGHT_PARENTHESIS',
    'TIMES', 'DOT', 'DIVIDE', 'EQUALS', 'LESS_THAN', 'GREATER_THAN', 'NOT',
    'PLUS_PLUS', 'MINUS_MINUS', 'EQUAL_EQUAL', 'AND', 'OR',
    'LESS_OR_EQUAL', 'GREATER_OR_EQUAL', 'NOT_EQUAL',
    'CHARACTER', 'NUMERAL', 'STRINGVAL', 'ID'
)


# -------------------------------
# Precedence and associativity
# Mirrors the declarations from parser.y (Bison)
# -------------------------------
precedence = (
    ('nonassoc', 'LOWER_THAN_ELSE'),
    ('nonassoc', 'ELSE'),
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'EQUAL_EQUAL', 'NOT_EQUAL'),
    ('left', 'LESS_THAN', 'LESS_OR_EQUAL', 'GREATER_THAN', 'GREATER_OR_EQUAL'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),
)


# -------------------------------
# Lexer adapter: converts our Token stream into PLY LexToken stream
# -------------------------------
class PlyLexerAdapter:
    def __init__(self, source_text: str):
        self._inner_lexer = Lexer(source_text)
        self._generator = self._inner_lexer.tokenize()

    def token(self):
        for tok in self._generator:
            # Stop when our lexer reports EOF to PLY (None)
            if tok.type == TokenType.EOF:
                return None

            # Convert to PLY's LexToken
            lt = LexToken()
            lt.type = tok.type.name
            lt.value = tok.lexeme
            lt.lineno = tok.line
            lt.lexpos = 0
            return lt
        return None


# -------------------------------
# Grammar rules (translated from parser.y to PLY)
# -------------------------------

# start symbol
start = 'programa'


def p_programa(p):
    """programa : lista_declaracoes_externas"""
    p[0] = p[1]


def p_lista_declaracoes_externas(p):
    """lista_declaracoes_externas : declaracao_externa
                                   | lista_declaracoes_externas declaracao_externa"""
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]


def p_declaracao_externa(p):
    """declaracao_externa : declaracao_tipo
                           | declaracao_funcao
                           | declaracao_variavel"""
    p[0] = p[1]


def p_tipo(p):
    """tipo : INTEGER
             | CHAR
             | BOOLEAN
             | STRING
             | idu"""
    p[0] = ('tipo', p[1])


def p_declaracao_tipo(p):
    """declaracao_tipo : TYPE idd EQUALS ARRAY LEFT_SQUARE NUMERAL RIGHT_SQUARE OF tipo
                        | TYPE idd EQUALS STRUCT new_block LEFT_BRACES declaracao_campos RIGHT_BRACES
                        | TYPE idd EQUALS tipo"""
    p[0] = ('decl_tipo',) + tuple(p[1:])


def p_declaracao_campos(p):
    """declaracao_campos : declaracao_campos SEMI_COLON lista_identificadores COLON tipo
                           | lista_identificadores COLON tipo"""
    p[0] = ('campos',) + tuple(p[1:])


def p_declaracao_funcao(p):
    """declaracao_funcao : FUNCTION idd new_block LEFT_PARENTHESIS lista_parametros RIGHT_PARENTHESIS COLON tipo bloco"""
    p[0] = ('funcao',) + tuple(p[1:])


def p_new_block(p):
    """new_block : """
    p[0] = ('new_block',)


def p_lista_parametros(p):
    """lista_parametros : lista_parametros COMMA idd COLON tipo
                         | idd COLON tipo"""
    if len(p) == 4:
        p[0] = [(p[1], p[3])]
    else:
        p[1].append((p[3], p[5]))
        p[0] = p[1]


def p_bloco(p):
    """bloco : LEFT_BRACES lista_declaracao_variaveis lista_comandos RIGHT_BRACES"""
    p[0] = ('bloco', p[2], p[3])


def p_lista_declaracao_variaveis(p):
    """lista_declaracao_variaveis : lista_declaracao_variaveis declaracao_variavel
                                   | declaracao_variavel"""
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]


def p_lista_comandos(p):
    """lista_comandos : lista_comandos comando
                       | comando"""
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]


def p_declaracao_variavel(p):
    """declaracao_variavel : VAR lista_identificadores COLON tipo SEMI_COLON"""
    p[0] = ('var', p[2], p[4])


def p_lista_identificadores(p):
    """lista_identificadores : lista_identificadores COMMA idd
                              | idd"""
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[3])
        p[0] = p[1]


def p_comando(p):
    """comando : IF LEFT_PARENTHESIS expressao RIGHT_PARENTHESIS comando %prec LOWER_THAN_ELSE
                | IF LEFT_PARENTHESIS expressao RIGHT_PARENTHESIS comando ELSE comando
                | WHILE LEFT_PARENTHESIS expressao RIGHT_PARENTHESIS comando
                | DO comando WHILE LEFT_PARENTHESIS expressao RIGHT_PARENTHESIS SEMI_COLON
                | bloco
                | valor_esquerdo EQUALS expressao SEMI_COLON
                | BREAK SEMI_COLON
                | CONTINUE SEMI_COLON"""
    p[0] = ('cmd',) + tuple(p[1:])


def p_expressao(p):
    """expressao : expressao AND expressao_l
                 | expressao OR expressao_l
                 | expressao_l"""
    p[0] = ('expr',) + tuple(p[1:])


def p_expressao_l(p):
    """expressao_l : expressao_l LESS_THAN expressao_r
                    | expressao_l LESS_OR_EQUAL expressao_r
                    | expressao_l GREATER_THAN expressao_r
                    | expressao_l GREATER_OR_EQUAL expressao_r
                    | expressao_l EQUAL_EQUAL expressao_r
                    | expressao_l NOT_EQUAL expressao_r
                    | expressao_r"""
    p[0] = ('expr_l',) + tuple(p[1:])


def p_expressao_r(p):
    """expressao_r : expressao_r PLUS expressao_y
                    | expressao_r MINUS expressao_y
                    | expressao_y"""
    p[0] = ('expr_r',) + tuple(p[1:])


def p_expressao_y(p):
    """expressao_y : expressao_y TIMES expressao_f
                    | expressao_y DIVIDE expressao_f
                    | expressao_f"""
    p[0] = ('expr_y',) + tuple(p[1:])


def p_expressao_f(p):
    """expressao_f : valor_esquerdo
                    | PLUS_PLUS valor_esquerdo
                    | MINUS_MINUS valor_esquerdo
                    | valor_esquerdo PLUS_PLUS
                    | valor_esquerdo MINUS_MINUS
                    | LEFT_PARENTHESIS expressao RIGHT_PARENTHESIS
                    | idu LEFT_PARENTHESIS lista_expressoes RIGHT_PARENTHESIS
                    | MINUS expressao_f %prec UMINUS
                    | NOT expressao_f
                    | true
                    | false
                    | chr
                    | str
                    | num"""
    p[0] = ('expr_f',) + tuple(p[1:])


def p_lista_expressoes(p):
    """lista_expressoes : lista_expressoes COMMA expressao
                         | expressao"""
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[3])
        p[0] = p[1]


def p_valor_esquerdo(p):
    """valor_esquerdo : valor_esquerdo DOT id
                       | valor_esquerdo LEFT_SQUARE expressao RIGHT_SQUARE
                       | idu"""
    if len(p) == 2:
        p[0] = ('lval_id', p[1])
    elif p[2] == '.':
        p[0] = ('lval_field', p[1], p[3])
    else:
        p[0] = ('lval_index', p[1], p[3])


def p_idd(p):
    """idd : ID"""
    p[0] = p[1]


def p_idu(p):
    """idu : ID"""
    p[0] = p[1]


def p_id(p):
    """id : ID"""
    p[0] = p[1]


def p_true(p):
    """true : TRUE"""
    p[0] = ('true',)


def p_false(p):
    """false : FALSE"""
    p[0] = ('false',)


def p_chr(p):
    """chr : CHARACTER"""
    p[0] = ('chr',)


def p_str(p):
    """str : STRINGVAL"""
    p[0] = ('str',)


def p_num(p):
    """num : NUMERAL"""
    p[0] = ('num',)


def p_error(p):
    if p is None:
        print('Syntax error: unexpected EOF')
        return
    print(f"Syntax error at line {p.lineno}: unexpected token {p.type} ('{p.value}')")


def build_parser(debug_out_path: str | None = 'parser.out'):
    parser = yacc.yacc(module=sys.modules[__name__], start='programa', debug=True, debugfile=debug_out_path, outputdir='.', write_tables=True)
    return parser


def parse_file(filename: str) -> None:
    with open(filename, 'r', encoding='utf-8') as f:
        source = f.read()
    adapter = PlyLexerAdapter(source)
    parser = build_parser()
    result = parser.parse(lexer=adapter)
    print('Parsing finished.')


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('Usage: python parser_ply.py <source_file>')
        sys.exit(1)
    parse_file(sys.argv[1])



