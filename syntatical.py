from lexical import Lexer
import csv
from t_attrib import T_attrib
from t_attrib import IDD, IDU, ID
from t_rules import T_rule
from scope_manager import ScopeManager
from typing import Generator
from tokens import Token
from t_nont import T_nont


class SyntaticalAnalyzer:
    def __init__(self, lexer: Lexer):
        self.lexer = lexer
        self.tokens: Generator[Token, None, None] = lexer.tokenize()
        self.stack: list[int] = []
        self.stack_sem: list[T_attrib] = []
        with open("lr_goto_and_action_items.csv", "r") as file:
            reader = csv.reader(file)
            self._action = list(reader)
        
        self.len_rules, self.left_side_of_rules = self._get_rules_info()
        self.scope_manager = ScopeManager()


    def push(self, item):
        if not isinstance(item, int):
            try:
                item = int(item)
                self.stack.append(item)
            except:
                print(f"Pushing item: {item} to stack")
        else:
            self.stack.append(item)
    
    
    def pop(self, n=1):
        for _ in range(n):
            self.stack.pop()
    
    
    def top(self):
        return self.stack[-1]
    

    def push_sem(self, attrib):
        self.stack_sem.append(attrib)
    
    
    def pop_sem(self, n=1):
        for _ in range(n):
            self.stack_sem.pop()


    def top_sem(self, offFromTop=0):
        return self.stack_sem[-1-offFromTop]
    
    
    def next_token(self):
        return next(self.tokens)


    def is_shift(self, p):
        return p > 0
    
    
    def is_reduce(self, p):
        return p < 0
    
    
    def rule(self, p):
        return -p
    
    
    def _get_rules_info(self):
        rules = []
        right_side_of_rules = []
        left_side_of_rules = []
        len_rules = []
        with open("parser.out", "r") as file:
            reader = file.read()
            
            for line in reader.split("\n"):
                if "Rule" in line:
                    rules.append(line)
                    
        for rule in rules:
            index = rule.find("->")
            right_side_of_rules.append(rule[index+2:])
            
            left = rule[:index]
            left = left.strip().split(" ")[-1]
            left_side_of_rules.append(left)

        for rule in right_side_of_rules:
            len_rules.append(len(rule.strip().split(" ")))
        
        return len_rules, left_side_of_rules
    
    
    def _find_index(self, token_type):
        for i, word in enumerate(self._action[0]):
            if word == token_type:
                return i
        
        print(f"Index not found for token type: {token_type}")
        return -1


    def action(self, q, a):
        index = self._find_index(a)
        state = self._action[q+1][index]
        print(f"Action called with q = {q} and a = {a} -> state = {state}")
        return state
    
    
    def analyze(self):
        q = 0
        self.push(0)
        self.current_token = self.next_token()
        
        while q != 1:
            print(f'pilha: {self.stack}')
            tok_name = '$end' if self.current_token.type.name == 'EOF' else self.current_token.type.name
            p = self.action(q, tok_name)
            try:
                p = int(p)
            except:
                p = 0

            if self.is_shift(p):
                self.push(p)
                self.current_token = self.next_token()
            elif self.is_reduce(p):
                r = self.rule(p)
                print(f"Reducing rule: {r}")
                self.pop(self.len_rules[r])
                self.push(self.action(self.top(), self.left_side_of_rules[r]))
                self.semantics(r)
            else:
                raise Exception(f"SytntaxError: {tok_name} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")

            q = self.top()
        
        print("\nSyntatical analysis completed successfully")


    def semantics(self, r: int):
        
        
        match r:
            case T_rule.PROGRAMA.value:
                pass
            case T_rule.LISTA_DECLARACOES_EXTERNAS.value:
                pass
            case T_rule.LISTA_DECLARACOES_EXTERNAS_REC.value:
                pass
            case T_rule.DECLARACAO_EXTERNA_TIPO.value:
                pass
            case T_rule.DECLARACAO_EXTERNA_FUNCAO.value:
                pass
            case T_rule.DECLARACAO_EXTERNA_VARIAVEL.value:
                pass
            case T_rule.TIPO_INTEGER.value:
                pass
            case T_rule.TIPO_CHAR.value:
                pass
            case T_rule.TIPO_BOOLEAN.value:
                pass
            case T_rule.TIPO_STRING.value:
                pass
            case T_rule.TIPO_IDU.value:
                pass
            case T_rule.DECLARACAO_TIPO_ARRAY.value:
                pass
            case T_rule.DECLARACAO_TIPO_STRUCT.value:
                pass
            case T_rule.DECLARACAO_TIPO.value:
                pass
            case T_rule.DECLARACAO_CAMPOS_REC.value:
                pass
            case T_rule.DECLARACAO_CAMPOS.value:
                pass
            case T_rule.DECLARACAO_FUNCAO.value:
                pass
            case T_rule.NEW_BLOCK.value:
                self.scope_manager.new_block()
            
            case T_rule.LISTA_PARAMETROS_REC.value:
                pass
            case T_rule.LISTA_PARAMETROS.value:
                pass
            case T_rule.BLOCO.value:
                pass
            case T_rule.LISTA_DECLARACAO_VARIAVEIS_REC.value:
                pass
            case T_rule.LISTA_DECLARACAO_VARIAVEIS.value:
                pass
            case T_rule.LISTA_COMANDOS_REC.value:
                pass
            case T_rule.LISTA_COMANDOS.value:
                pass
            case T_rule.DECLARACAO_VARIAVEL.value:
                pass
            case T_rule.LISTA_IDENTIFICADORES_REC.value:
                pass
            case T_rule.LISTA_IDENTIFICADORES.value:
                pass
            case T_rule.COMANDO_IF.value:
                pass
            case T_rule.COMANDO_IF_ELSE.value:
                pass
            case T_rule.COMANDO_WHILE.value:
                pass
            case T_rule.COMANDO_DO.value:
                pass
            case T_rule.COMANDO_BLOCO.value:
                pass
            case T_rule.COMANDO_LEFT_VALUE_EQUALS.value:
                pass
            case T_rule.COMANDO_BREAK.value:
                pass
            case T_rule.COMANDO_CONTINUE.value:
                pass
            case T_rule.EXPRESSAO_AND.value:
                pass
            case T_rule.EXPRESSAO_OR.value:
                pass
            case T_rule.EXPRESSAO.value:
                pass
            case T_rule.EXPRESSAO_L_LESS_THAN.value:
                pass
            case T_rule.EXPRESSAO_L_LESS_OR_EQUAL.value:
                pass
            case T_rule.EXPRESSAO_L_GREATER_THAN.value:
                pass
            case T_rule.EXPRESSAO_L_GREATER_OR_EQUAL.value:
                pass
            case T_rule.EXPRESSAO_L_EQUAL_EQUAL.value:
                pass
            case T_rule.EXPRESSAO_L_NOT_EQUAL.value:
                pass
            case T_rule.EXPRESSAO_L_R.value:
                pass
            case T_rule.EXPRESSAO_R_PLUS.value:
                pass
            case T_rule.EXPRESSAO_R_MINUS.value:
                pass
            case T_rule.EXPRESSAO_R_Y.value:
                pass
            case T_rule.EXPRESSAO_Y_TIMES.value:
                pass
            case T_rule.EXPRESSAO_Y_DIVIDE.value:
                pass
            case T_rule.EXPRESSAO_Y.value:
                pass
            case T_rule.EXPRESSAO_F_LEFT_VALUE.value:
                pass
            case T_rule.EXPRESSAO_F_PLUS_PLUS_LEFT_VALUE.value:
                pass
            case T_rule.EXPRESSAO_F_MINUS_MINUS_LEFT_VALUE.value:
                pass
            case T_rule.EXPRESSAO_F_LEFT_VALUE_PLUS_PLUS.value:
                pass
            case T_rule.EXPRESSAO_F_LEFT_VALUE_MINUS_MINUS.value:
                pass
            case T_rule.EXPRESSAO_F_LEFT_PARENTHESIS.value:
                pass
            case T_rule.EXPRESSAO_F_IDU_LEFT_PARENTHESIS.value:
                pass
            case T_rule.EXPRESSAO_F_MINUS.value:
                pass
            case T_rule.EXPRESSAO_F_NOT.value:
                pass
            case T_rule.EXPRESSAO_F_TRUE.value:
                pass
            case T_rule.EXPRESSAO_F_FALSE.value:
                pass
            case T_rule.EXPRESSAO_F_CHARACTER.value:
                pass
            case T_rule.EXPRESSAO_F_STRINGVAL.value:
                pass
            case T_rule.EXPRESSAO_F_NUMERAL.value:
                pass
            case T_rule.LISTA_EXPRESSOES_REC.value:
                pass
            case T_rule.LISTA_EXPRESSOES.value:
                pass
            case T_rule.VALOR_ESQUERDO_DOT.value:
                pass
            case T_rule.VALOR_ESQUERDO_LEFT_SQUARE.value:
                pass
            case T_rule.VALOR_ESQUERDO_IDU.value:
                pass
            
            case T_rule.IDD.value:
                name = self.current_token.secondary_token
                idd_sem: T_attrib = T_attrib(T_nont.IDD, IDD(name, None))
                
                if self.scope_manager.search(name) is not None:
                    print(f"SemanticError: Variable {name} already defined in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                else:
                    idd_sem.attrib.obj = self.scope_manager.define(name)
                    
                self.push_sem(idd_sem)
                
            case T_rule.IDU.value:
                name = self.current_token.secondary_token
                idu_sem: T_attrib = T_attrib(T_nont.IDU, IDU(name, None))
                
                p = self.scope_manager.find(name)
                
                if p is None:
                    print(f"SemanticError: Variable {name} not defined in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                    p = self.scope_manager.define(name)
                
                idu_sem.attrib.obj = p
                self.push_sem(idu_sem)
            
            case T_rule.ID.value:
                name = self.current_token.secondary_token
                id_sem: T_attrib = T_attrib(T_nont.ID, ID(name, None))
                
                self.push_sem(id_sem)
            
            case T_rule.TRUE.value:
                pass
            case T_rule.FALSE.value:
                pass
            case T_rule.CHR.value:
                pass
            case T_rule.STR.value:
                pass
            case T_rule.NUM.value:
                pass