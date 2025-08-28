from enum import Enum


T_token = Enum(
    value = 'T_token', 
    names = [
        # Palavras reservadas
        'ARRAY_', 'BOOLEAN_', 'BREAK_', 'CHAR_', 'CONTINUE_', 'DO_',
        'ELSE_', 'FALSE_', 'FUNCTION_', 'IF_', 'INTEGER_', 'OF_',
        'STRING_', 'STRUCT_', 'TRUE_', 'TYPE_', 'VAR_', 'WHILE_',
        
        # Simbolos
        'COLON_', 'SEMI_COLON_', 'COMMA_', 'EQUALS_', 'LEFT_SQUARE_', 'RIGHT_SQUARE_',
        'LEFT_BRACES_', 'RIGHT_BRACES_', 'LEFT_PARENTHESIS_', 'RIGHT_PARENTHESIS_', 'AND_',
        'OR_', 'LESS_THAN_', 'GREATER_THAN_', 'LESS_OR_EQUAL_', 'GREATER_OR_EQUAL_',
        'NOT_EQUAL_', 'EQUAL_EQUAL_', 'PLUS_', 'PLUS_PLUS_', 'MINUS_', 'MINUS_MINUS_', 'TIMES_',
        'DIVIDE_', 'DOT_', 'NOT_',
        
        
        # Tokens regulares
        'CHARACTER_', 'NUMBER_', 'STRINGVAL_', 'ID_',
        
        #token desconhecido
        'UNKNOWN_'
    ],
    start = 0,
    type = int
)

if __name__ == "__main__":
    print(f'{T_token.ARRAY_.name}: {T_token.ARRAY_.value}')
    print(f'{T_token.BOOLEAN_.name}: {T_token.BOOLEAN_.value}')
    print(f'{T_token.BREAK_.name}: {T_token.BREAK_.value}')
    print(f'{T_token.CHAR_.name}: {T_token.CHAR_.value}')
    print(f'{T_token.CONTINUE_.name}: {T_token.CONTINUE_.value}')
    print(f'{T_token.DO_.name}: {T_token.DO_.value}')
    print(f'{T_token.ELSE_.name}: {T_token.ELSE_.value}')
    print(f'{T_token.FALSE_.name}: {T_token.FALSE_.value}')
    print(f'{T_token.FUNCTION_.name}: {T_token.FUNCTION_.value}')
    print(f'{T_token.IF_.name}: {T_token.IF_.value}')
    print(f'{T_token.INTEGER_.name}: {T_token.INTEGER_.value}')
    print(f'{T_token.OF_.name}: {T_token.OF_.value}')
    print(f'{T_token.STRING_.name}: {T_token.STRING_.value}')
    print(f'{T_token.STRUCT_.name}: {T_token.STRUCT_.value}')
    print(f'{T_token.TRUE_.name}: {T_token.TRUE_.value}')
    print(f'{T_token.TYPE_.name}: {T_token.TYPE_.value}')
    print(f'{T_token.VAR_.name}: {T_token.VAR_.value}')
    print(f'{T_token.WHILE_.name}: {T_token.WHILE_.value}')