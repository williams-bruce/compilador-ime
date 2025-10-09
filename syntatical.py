from lexical import Lexer
import csv
from t_attrib import T_attrib
from t_attrib import IDD, IDU, ID, TIPO, LISTA_IDENTIFICADORES, DECLARACAO_VARIAVEL
from t_attrib import TRUE, FALSE, CHR, STR, NUM, DECLARACAO_TIPO, DECLARACAO_CAMPOS
from t_attrib import LISTA_PARAMETROS, DECLARACAO_FUNCAO, EXPRESSAO, EXPRESSAO_L
from t_attrib import EXPRESSAO_R, EXPRESSAO_Y, EXPRESSAO_F, VALOR_ESQUERDO
from t_attrib import MARCADOR_C, LISTA_EXPRESSOES
from t_rules import T_rule
from scope_manager import ScopeManager
from typing import Generator, Optional
from tokens import Token
from t_nont import T_nont
from object import Object, Var, Array, Alias, Field, Struct, Param, Function
from t_kind import T_kind


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
        
        self.int_ : Object = Object(nName=-1, pNext=None, ekind=T_kind.SCALAR_TYPE_)
        self.char_ : Object = Object(nName=-1, pNext=None, ekind=T_kind.SCALAR_TYPE_)
        self.bool_ : Object = Object(nName=-1, pNext=None, ekind=T_kind.SCALAR_TYPE_)
        self.string_ : Object = Object(nName=-1, pNext=None, ekind=T_kind.SCALAR_TYPE_)
        self.universal_ : Object = Object(nName=-1, pNext=None, ekind=T_kind.UNIVERSAL_)


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
                idu = T_attrib(T_nont.TIPO_, TIPO(type = self.int_))
                self.push_sem(idu)
            
            case T_rule.TIPO_CHAR.value:
                idu = T_attrib(T_nont.TIPO_, TIPO(type = self.char_))
                self.push_sem(idu)
            
            case T_rule.TIPO_BOOLEAN.value:
                idu = T_attrib(T_nont.TIPO_, TIPO(type = self.bool_))
                self.push_sem(idu)
            
            case T_rule.TIPO_STRING.value:
                idu = T_attrib(T_nont.TIPO_, TIPO(type = self.string_))
                self.push_sem(idu)
            
            case T_rule.TIPO_IDU.value:
                t: T_attrib = T_attrib(T_nont.TIPO_, TIPO(type = self.universal_))
                idu: T_attrib = self.top_sem()
                self.pop_sem()
                p: Object = idu.attrib.obj
                p = p.kind_info
                if self.is_type_kind(p.eKind) or p.eKind == T_kind.UNIVERSAL_:
                    t.attrib.type = p
                else:
                    print(f"SemanticError: Type {p.eKind} is not a valid type in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                self.push_sem(t)
            
            case T_rule.DECLARACAO_TIPO_ARRAY.value:
                t = self.top_sem()
                num = self.top_sem(1)
                idd = self.top_sem(2)
                self.pop_sem(3)
                
                p = idd.attrib.obj
                p.eKind = T_kind.ARRAY_TYPE_
                p.kind_info = Array(pElemType = t.attrib.type, nNumElens = num.attrib.val)
                
                dt: T_attrib = T_attrib(
                    T_nont.DECLARACAO_TIPO_, 
                    DECLARACAO_TIPO(
                        idd.attrib.obj,
                    )
                )
                self.push_sem(dt)
            
            case T_rule.DECLARACAO_TIPO_STRUCT.value:
                dc = self.top_sem()
                nb = self.top_sem(1)
                idd = self.top_sem(2)
                self.pop_sem(3)
                
                p = idd.attrib.obj
                p.eKind = T_kind.STRUCT_TYPE_
                p.kind_info = Struct(pFields = dc.attrib.list)
                
                self.scope_manager.end_block()
                
                dt: T_attrib = T_attrib(
                    T_nont.DECLARACAO_TIPO_, 
                    DECLARACAO_TIPO(
                        idd.attrib.obj,
                    )
                )
                self.push_sem(dt)
                
            case T_rule.DECLARACAO_TIPO.value:
                t = self.top_sem()
                idd = self.top_sem(1)
                self.pop_sem(2)
                
                p = idd.attrib.obj
                p.eKind = T_kind.ALIAS_TYPE_
                p.kind_info = Alias(pBaseType = t.attrib.type)
                
                dt: T_attrib = T_attrib(
                    T_nont.DECLARACAO_TIPO_, 
                    DECLARACAO_TIPO(
                        idd.attrib.obj,
                    )
                )
                self.push_sem(dt)
            
            case T_rule.DECLARACAO_CAMPOS_REC.value:
                t = self.top_sem()
                li = self.top_sem(1)
                dc1 = self.top_sem(2)
                self.pop_sem(3)
                
                p = li.attrib.list
                while p is not None and p.eKind == T_kind.NO_KIND_DEF_:
                    p.eKind = T_kind.FIELD_
                    p.kind_info = Field(pType = t.attrib.type)
                    p = p.pNext
                
                dc0: T_attrib = T_attrib(
                    T_nont.DECLARACAO_CAMPOS_,
                    DECLARACAO_CAMPOS(dc1.attrib.list)
                )
                self.push_sem(dc0)
            
            case T_rule.DECLARACAO_CAMPOS.value:
                t = self.top_sem()
                li = self.top_sem(1)
                self.pop_sem(2)

                p = li.attrib.list
                while p is not None and p.eKind == T_kind.NO_KIND_DEF_:
                    p.eKind = T_kind.FIELD_
                    p.kind_info = Field(pType = t.attrib.type)
                    p = p.pNext

                dc: T_attrib = T_attrib(
                    T_nont.DECLARACAO_CAMPOS_, 
                    DECLARACAO_CAMPOS(
                        li.attrib.list
                    )
                )
                self.push_sem(dc)

            case T_rule.DECLARACAO_FUNCAO.value:
                mf = self.top_sem()
                self.pop_sem()
                
                self.scope_manager.end_block()
                
                df: T_attrib = T_attrib(
                    T_nont.DECLARACAO_FUNCAO_,
                    DECLARACAO_FUNCAO(mf.attrib.obj)
                )
                self.push_sem(df)
            
            case T_rule.NEW_BLOCK.value:
                self.scope_manager.new_block()
                
                nb: T_attrib = T_attrib(
                    t_nont= T_nont.NEW_BLOCK_,
                    attrib=None
                )
                self.push_sem(nb)
            
            
            case T_rule.MARCADOR_FUNCAO.value:
                t = self.top_sem()
                lp = self.top_sem(1)
                nb = self.top_sem(2)
                idd = self.top_sem(3)
                self.pop_sem(4)
                
                f = idd.attrib.obj
                f.eKind = T_kind.FUNCTION_
                f.kind_info = Function(
                    pRetType = t.attrib.type,
                    pParams = lp.attrib.list
                )
                
                mf: T_attrib = T_attrib(
                    T_nont.MARCADOR_FUNCAO_,
                    None
                )
                self.push_sem(mf)
            
            case T_rule.MARCADOR_C.value:
                idu: T_attrib = self.top_sem()
                self.pop_sem()
                mc:T_attrib = T_attrib(
                    t_nont=T_nont.MARCADOR_C,
                    attrib=None
                )
                
                fun: Object = idu.attrib.obj
                
                if fun.eKind != T_kind.FUNCTION_:
                    print(f"SemanticError: Kind not function in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                    mc.attrib = MARCADOR_C(
                        type=self.universal_,
                        param=None,
                        err=True
                    )
                else:
                    mc.attrib = MARCADOR_C(
                        type=fun.kind_info.pRetType,
                        param=fun.kind_info.pParams,
                        err=False
                    )
                
                self.push_sem(mc)
            
            case T_rule.LISTA_PARAMETROS_REC.value:
                t = self.top_sem()
                idd = self.top_sem(1)
                lp1 = self.top_sem(2)
                self.pop_sem(3)
                
                p = idd.attrib.obj
                p.eKind = T_kind.PARAM_
                p.kind_info = Param(pType = t.attrib.type)
                
                lp0: T_attrib = T_attrib(
                    T_nont.LISTA_PARAMETROS_,
                    LISTA_PARAMETROS(lp1.attrib.list)
                )
                self.push_sem(lp0)
            
            case T_rule.LISTA_PARAMETROS.value:
                t = self.top_sem()
                idd = self.top_sem(1)
                self.pop_sem(2)
                
                p = idd.attrib.obj
                p.eKind = T_kind.PARAM_
                p.kind_info = Param(pType = t.attrib.type)
                
                lp: T_attrib = T_attrib(
                    T_nont.LISTA_PARAMETROS_,
                    LISTA_PARAMETROS(
                        idd.attrib.obj
                    )
                )
                self.push_sem(lp)
            
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
                t: T_attrib = self.top_sem()
                li: T_attrib = self.top_sem(1)
                self.pop_sem(2)
                p: Optional[Object] = li.attrib.list

                while p is not None and p.eKind == T_kind.NO_KIND_DEF_:
                    p.eKind = T_kind.VAR_
                    p.kind_info = Var(pType = t.attrib.type)
                    p = p.pNext
                
                dv: T_attrib = T_attrib(
                    T_nont.DECLARACAO_VARIAVEL_,
                    DECLARACAO_VARIAVEL(li.attrib.list)
                )
                
                self.push_sem(dv)
            
            case T_rule.LISTA_IDENTIFICADORES_REC.value:
                li1: T_attrib = self.top_sem(1)
                self.pop_sem(2)
                
                li0: T_attrib = T_attrib(T_nont.LISTA_IDENTIFICADORES_, LISTA_IDENTIFICADORES(li1.attrib.list))
                self.push_sem(li0)
            
            case T_rule.LISTA_IDENTIFICADORES.value:
                idd: T_attrib = self.top_sem()
                self.pop_sem()
                li: T_attrib = T_attrib(T_nont.LISTA_IDENTIFICADORES_, LISTA_IDENTIFICADORES(idd.attrib.obj))
                self.push_sem(li)
                
            case T_rule.COMANDO_IF.value:
                e = self.top_sem(1)
                # pop the expression and the command
                self.pop_sem(2)
                
                t: Object = e.attrib.type
                
                if not self.check_types(t, self.bool_):
                    print(f"SemanticError: Type {t.eKind} is not a valid type. Expected type: BOOLEAN in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                s: T_attrib = T_attrib(
                    T_nont.COMANDO_,
                    None
                )
                self.push_sem(s)
            
            case T_rule.COMANDO_IF_ELSE.value:
                e = self.top_sem(2)
                # pop the expression and the commands
                self.pop_sem(3)
                t: Object = e.attrib.type
                
                if not self.check_types(t, self.bool_):
                    print(f"SemanticError: Type {t.eKind} is not a valid type. Expected type: BOOLEAN in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                s: T_attrib = T_attrib(
                    T_nont.COMANDO_,
                    None
                )
                self.push_sem(s)
            
            case T_rule.COMANDO_WHILE.value:
                e = self.top_sem(1)
                # pop the expression and the command
                self.pop_sem(2)
                
                t: Object = e.attrib.type
                
                if not self.check_types(t, self.bool_):
                    print(f"SemanticError: Type {t.eKind} is not a valid type. Expected type: BOOLEAN in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                s: T_attrib = T_attrib(
                    T_nont.COMANDO_,
                    None
                )
                self.push_sem(s)
            
            case T_rule.COMANDO_DO.value:
                e = self.top_sem()
                # pop the expression and the command
                self.pop_sem(2)
                
                t: Object = e.attrib.type
                
                if not self.check_types(t, self.bool_):
                    print(f"SemanticError: Type {t.eKind} is not a valid type. Expected type: BOOLEAN in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                s: T_attrib = T_attrib(
                    T_nont.COMANDO_,
                    None
                )
                self.push_sem(s)
            
            case T_rule.COMANDO_BLOCO.value:
                # pop the block and new block
                self.pop_sem(2)
                
                self.scope_manager.end_block()
                
                s: T_attrib = T_attrib(
                    T_nont.COMANDO_,
                    None
                )
                self.push_sem(s)
            
            case T_rule.COMANDO_LEFT_VALUE_EQUALS.value:
                e = self.top_sem()
                lv = self.top_sem(1)
                self.pop_sem(2)
                
                t1: Object = lv.attrib.type
                t2: Object = e.attrib.type
                
                if not self.check_types(t1, t2):
                    print(f"SemanticError: Type mismatch. Left value type: {t1.eKind}. Expression type: {t2.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                s: T_attrib = T_attrib(
                    T_nont.COMANDO_,
                    None
                )
                self.push_sem(s)
            
            case T_rule.COMANDO_BREAK.value:
                pass
            
            case T_rule.COMANDO_CONTINUE.value:
                pass
            
            case T_rule.EXPRESSAO_AND.value:
                l = self.top_sem()
                e1 = self.top_sem(1)
                self.pop_sem(2)
                
                if not self.check_types(e1.attrib.type, self.bool_):
                    print(f"SemanticError: Type {e1.attrib.type.eKind} is not a valid type. Expected type: BOOLEAN in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                if not self.check_types(l.attrib.type, self.bool_):
                    print(f"SemanticError: Type {l.attrib.type.eKind} is not a valid type. Expected type: BOOLEAN in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                e0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_,
                    attrib=EXPRESSAO(e1.attrib.type)
                )
                
                self.push_sem(e0)
            
            case T_rule.EXPRESSAO_OR.value:
                l = self.top_sem()
                e1 = self.top_sem(1)
                self.pop_sem(2)
                
                if not self.check_types(e1.attrib.type, self.bool_):
                    print(f"SemanticError: Type {e1.attrib.type.eKind} is not a valid type. Expected type: BOOLEAN in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                if not self.check_types(l.attrib.type, self.bool_):
                    print(f"SemanticError: Type {l.attrib.type.eKind} is not a valid type. Expected type: BOOLEAN in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                e0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_,
                    attrib=EXPRESSAO(e1.attrib.type)
                )
                
                self.push_sem(e0)
            
            case T_rule.EXPRESSAO.value:
                l = self.top_sem()
                self.pop_sem()

                e: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_,
                    attrib=EXPRESSAO(l.attrib.type)
                )
                
                self.push_sem(e)
            
            case T_rule.EXPRESSAO_L_LESS_THAN.value:
                r: T_attrib = self.top_sem()
                l1 = self.top_sem(1)
                self.pop_sem(2)
                
                if not self.check_types(l1.attrib.type, r.attrib.type):
                    print(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                l0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_L_,
                    attrib=EXPRESSAO_L(type=self.bool_)
                )
                
                self.push_sem(l0)
            
            case T_rule.EXPRESSAO_L_LESS_OR_EQUAL.value:
                r: T_attrib = self.top_sem()
                l1 = self.top_sem(1)
                self.pop_sem(2)
                
                if not self.check_types(l1.attrib.type, r.attrib.type):
                    print(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                l0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_L_,
                    attrib=EXPRESSAO_L(type=self.bool_)
                )
                
                self.push_sem(l0)
            
            case T_rule.EXPRESSAO_L_GREATER_THAN.value:
                r: T_attrib = self.top_sem()
                l1 = self.top_sem(1)
                self.pop_sem(2)
                
                if not self.check_types(l1.attrib.type, r.attrib.type):
                    print(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                l0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_L_,
                    attrib=EXPRESSAO_L(type=self.bool_)
                )
                
                self.push_sem(l0)
            
            case T_rule.EXPRESSAO_L_GREATER_OR_EQUAL.value:
                r: T_attrib = self.top_sem()
                l1 = self.top_sem(1)
                self.pop_sem(2)
                
                if not self.check_types(l1.attrib.type, r.attrib.type):
                    print(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                l0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_L_,
                    attrib=EXPRESSAO_L(type=self.bool_)
                )
                
                self.push_sem(l0)
            
            case T_rule.EXPRESSAO_L_EQUAL_EQUAL.value:
                r: T_attrib = self.top_sem()
                l1 = self.top_sem(1)
                self.pop_sem(2)
                
                if not self.check_types(l1.attrib.type, r.attrib.type):
                    print(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                l0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_L_,
                    attrib=EXPRESSAO_L(type=self.bool_)
                )
                
                self.push_sem(l0)
            
            case T_rule.EXPRESSAO_L_NOT_EQUAL.value:
                r: T_attrib = self.top_sem()
                l1 = self.top_sem(1)
                self.pop_sem(2)
                
                if not self.check_types(l1.attrib.type, r.attrib.type):
                    print(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                l0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_L_,
                    attrib=EXPRESSAO_L(type=self.bool_)
                )
                
                self.push_sem(l0)
            
            case T_rule.EXPRESSAO_L_R.value:
                r: T_attrib = self.top_sem()
                self.pop_sem()

                l: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_L_,
                    attrib=EXPRESSAO_L(type=r.attrib.type)
                )
                
                self.push_sem(l)
            
            case T_rule.EXPRESSAO_R_PLUS.value:
                y: T_attrib = self.top_sem()
                r1: T_attrib = self.top_sem(1)
                self.pop_sem(2)

                if not self.check_types(y.attrib.type, r1.attrib.type):
                    print(f"SemanticError: Type mismatch. expressao_y type: {y.attrib.type.eKind}. expressao_r type: {r1.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                if not self.check_types(r1.attrib.type.eKind, self.int_) and not self.check_types(r1.attrib.type.eKind, self.string_):
                    print(f"SemanticError: Invalid Type. expressao_r type: {y.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                r0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_R_,
                    attrib=EXPRESSAO_R(type=r1.attrib.type)
                )
                
                self.push_sem(r0)
            
            case T_rule.EXPRESSAO_R_MINUS.value:
                y: T_attrib = self.top_sem()
                r1: T_attrib = self.top_sem(1)
                self.pop_sem(2)

                if not self.check_types(y.attrib.type, r1.attrib.type):
                    print(f"SemanticError: Type mismatch. expressao_y type: {y.attrib.type.eKind}. expressao_r type: {r1.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                if not self.check_types(r1.attrib.type.eKind, self.int_) and not self.check_types(r1.attrib.type.eKind, self.string_):
                    print(f"SemanticError: Invalid Type. expressao_r type: {y.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                r0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_R_,
                    attrib=EXPRESSAO_R(type=r1.attrib.type)
                )
                
                self.push_sem(r0)
            
            case T_rule.EXPRESSAO_R_Y.value:
                y: T_attrib = self.top_sem()
                self.pop_sem()
                
                r: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_R_,
                    attrib=EXPRESSAO_R(type=y.attrib.type)
                )
                
                self.push_sem(r)
            
            case T_rule.EXPRESSAO_Y_TIMES.value:
                f: T_attrib = self.top_sem()
                y1: T_attrib = self.top_sem(1)
                self.pop_sem(2)

                if not self.check_types(f.attrib.type, y1.attrib.type):
                    print(f"SemanticError: Type mismatch. expressao_y type: {f.attrib.type.eKind}. expressao_f type: {y1.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                if not self.check_types(y1.attrib.type.eKind, self.int_):
                    print(f"SemanticError: Invalid Type. expressao_y type: {f.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                y0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_Y_,
                    attrib=EXPRESSAO_Y(type=y1.attrib.type)
                )
                
                self.push_sem(y0)
            
            case T_rule.EXPRESSAO_Y_DIVIDE.value:
                f: T_attrib = self.top_sem()
                y1: T_attrib = self.top_sem(1)
                self.pop_sem(2)

                if not self.check_types(f.attrib.type, y1.attrib.type):
                    print(f"SemanticError: Type mismatch. expressao_y type: {f.attrib.type.eKind}. expressao_f type: {y1.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")

                if not self.check_types(y1.attrib.type.eKind, self.int_):
                    print(f"SemanticError: Invalid Type. expressao_y type: {f.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")

                y0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_Y_,
                    attrib=EXPRESSAO_Y(type=y1.attrib.type)
                )

                self.push_sem(y0)

            case T_rule.EXPRESSAO_Y.value:
                f: T_attrib = self.top_sem()
                self.pop_sem()

                y: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_Y_,
                    attrib=EXPRESSAO_Y(type=f.attrib.type)
                )

                self.push_sem(y)
            
            case T_rule.EXPRESSAO_F_LEFT_VALUE.value:
                lv: T_attrib = self.top_sem()
                self.pop_sem()

                f: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=lv.attrib.type)
                )
                
                self.push_sem(f)
            
            case T_rule.EXPRESSAO_F_PLUS_PLUS_LEFT_VALUE.value:
                lv: T_attrib = self.top_sem()
                self.pop_sem()
                
                if not self.check_types(lv.attrib.type, self.int_):
                    print(f"SemanticError: Invalid Type. expressao_y type: {lv.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                f: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )
                
                self.push_sem(f)
            
            case T_rule.EXPRESSAO_F_MINUS_MINUS_LEFT_VALUE.value:
                lv: T_attrib = self.top_sem()
                self.pop_sem()
                
                if not self.check_types(lv.attrib.type, self.int_):
                    print(f"SemanticError: Invalid Type. expressao_y type: {lv.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                f: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )
                
                self.push_sem(f)
            
            case T_rule.EXPRESSAO_F_LEFT_VALUE_PLUS_PLUS.value:
                lv: T_attrib = self.top_sem()
                self.pop_sem()
                
                if not self.check_types(lv.attrib.type, self.int_):
                    print(f"SemanticError: Invalid Type. expressao_y type: {lv.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                f: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )
                
                self.push_sem(f)
            
            case T_rule.EXPRESSAO_F_LEFT_VALUE_MINUS_MINUS.value:
                lv: T_attrib = self.top_sem()
                self.pop_sem()
                
                if not self.check_types(lv.attrib.type, self.int_):
                    print(f"SemanticError: Invalid Type. expressao_y type: {lv.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                f: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )

                self.push_sem(f)

            case T_rule.EXPRESSAO_F_LEFT_PARENTHESIS.value:
                e: T_attrib = self.top_sem()
                self.pop_sem()

                f: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=e.attrib.type)
                )

                self.push_sem(f)

            case T_rule.EXPRESSAO_F_IDU_LEFT_PARENTHESIS.value:
                le = self.top_sem()
                mc = self.top_sem(1)
                self.pop_sem(2)
                f = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=mc.attrib.type)
                )

                if not mc.attrib.err:
                    if le.attrib.param is not None:
                        print(f"SemanticError: LE with too few args in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")

            case T_rule.EXPRESSAO_F_MINUS.value:
                f1 = self.top_sem()
                self.pop_sem()

                if not self.check_types(f1.attrib.type, self.int_):
                    print(f"SemanticError: Invalid Type. expressao_f type: {f1.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")

                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_NOT.value:
                f1 = self.top_sem()
                self.pop_sem()

                if not self.check_types(f1.attrib.type, self.bool_):
                    print(f"SemanticError: Invalid Type. expressao_f type: {f1.attrib.type.eKind} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")

                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.bool_)
                )

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_TRUE.value:
                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.bool_)
                )

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_FALSE.value:
                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.bool_)
                )

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_CHARACTER.value:
                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.char_)
                )

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_STRINGVAL.value:
                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.string_)
                )

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_NUMERAL.value:
                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )

                self.push_sem(f0)
            
            case T_rule.LISTA_EXPRESSOES_REC.value:
                e = self.top_sem()
                le1 = self.top_sem(1)
                self.pop_sem(2)
                le0 = T_attrib(
                    t_nont=T_nont.LISTA_EXPRESSOES_,
                    attrib=LISTA_EXPRESSOES(
                        param=None,
                        err=le1.attrib.err,
                        n = 0
                    )
                )
                n = le1.attrib.n
                
                if not le1.attrib.err:
                    p = le1.attrib.param
                    if p is None:
                        print(f"SemanticError: LE with too many args in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                        le0.attrib.err = True
                    else:
                        if not self.check_types(p.kind_info.pType, e.attrib.type):
                            print(f"SemanticError: Error param type {n} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                        le0.attrib.param = p.pNext
                        le0.attrib.n = n + 1
                        
            
            case T_rule.LISTA_EXPRESSOES.value:
                e: T_attrib = self.top_sem()
                mc: T_attrib = self.top_sem(1)
                self.pop_sem(2)
                
                le: T_attrib = T_attrib(
                    t_nont=T_nont.LISTA_EXPRESSOES_,
                    attrib=LISTA_EXPRESSOES(
                        param=None,
                        err = mc.attrib.err,
                        n=0
                    )
                )
                n: int = 1
                
                if not mc.attrib.err:
                    p = mc.attrib.param
                    if p is None:
                        print(f"SemanticError: LE with too many args in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                        le.attrib.err = True
                    else:
                        if not self.check_types(p.kind_info.pType, e.attrib.type):
                            print(f"SemanticError: Error param type {n} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                        le.attrib.param = p.pNext
                        le.attrib.n = n + 1
                
                self.push_sem(le)
            
            case T_rule.VALOR_ESQUERDO_DOT.value:
                id: T_attrib = self.top_sem()
                lv1 = self.top_sem(1)
                self.pop_sem(2)
                
                lv0: T_attrib = T_attrib(
                    t_nont=T_nont.VALOR_ESQUERDO_,
                    attrib=None
                )
                t = lv1.attrib.type
                
                if t.eKind != T_kind.STRUCT_TYPE_:
                    if t.eKind != T_kind.UNIVERSAL_:
                        print(f"SemanticError: Kind not struct in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                    lv0.attrib = VALOR_ESQUERDO(type=self.universal_)
                else:
                    p = t.kind_info.pFields
                    while p is not None:
                        if p.nName == id.attrib.name:
                            break
                        p = p.pNext
                    if p is None:
                        print(f"SemanticError: Field not declared in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                        lv0.attrib.type = self.universal_
                    else:
                        lv0.attrib.type = p.kind_info.pType
                        
            
            case T_rule.VALOR_ESQUERDO_LEFT_SQUARE.value:
                e: T_attrib = self.top_sem()
                lv1 = self.top_sem(1)
                self.pop_sem(2)

                lv0 = T_attrib(
                    t_nont=T_nont.VALOR_ESQUERDO_,
                    attrib=None
                )
                
                t = lv1.attrib.type
                
                if t == self.string_:
                    lv0.attrib = VALOR_ESQUERDO(type=self.char_)
                elif t.eKind != T_kind.ARRAY_TYPE_:
                    if t.eKind != T_kind.UNIVERSAL_:
                        print(f"SemanticError: Kind not array in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                    lv0.attrib = VALOR_ESQUERDO(type=self.universal_)
                else:
                    lv0.attrib = VALOR_ESQUERDO(type=t.kind_info.pElemType)
                
                if not self.check_types(e.attrib.type, self.int_):
                    print(f"SemanticError: Invalid index type in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                
                self.push_sem(lv0)

            case T_rule.VALOR_ESQUERDO_IDU.value:
                idu = self.top_sem()
                self.pop_sem()
                p = idu.attrib.obj
                
                lv: T_attrib = T_attrib(
                    t_nont=T_nont.VALOR_ESQUERDO_,
                    attrib=None
                )
                
                if p.eKind != T_kind.VAR_ and p.eKind != T_kind.PARAM_:
                    if p.eKind != T_kind.UNIVERSAL_:
                        print(f"SemanticError: Kind not VAR in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                    lv.attrib = VALOR_ESQUERDO(type=self.universal_)
                else:
                    lv.attrib = VALOR_ESQUERDO(type=p.kind_info.pType)
                
                self.push_sem(lv)
            
            case T_rule.IDD.value:
                name = self.current_token.secondary_token
                
                p = self.scope_manager.search(name)
                if p is not None:
                    print(f"SemanticError: Variable {name} already defined in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                else:
                    p = self.scope_manager.define(name)
                
                p.eKind = T_kind.NO_KIND_DEF_
                idd: T_attrib = T_attrib(T_nont.IDD_, IDD(name, p))
                    
                self.push_sem(idd)
                
            case T_rule.IDU.value:
                name = self.current_token.secondary_token
                idu: T_attrib = T_attrib(T_nont.IDU_, IDU(name, None))
                
                p = self.scope_manager.find(name)
                
                if p is None:
                    print(f"SemanticError: Variable {name} not defined in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                    p = self.scope_manager.define(name)
                
                idu.attrib.obj = p
                self.push_sem(idu)
            
            case T_rule.ID.value:
                name = self.current_token.secondary_token
                id: T_attrib = T_attrib(T_nont.ID_, ID(name, None))
                
                self.push_sem(id)
            
            case T_rule.TRUE.value:
                true_sem: T_attrib = T_attrib(
                    T_nont.TRUE_, 
                    TRUE(
                        type = self.bool_,
                        val = True
                    )
                )
                self.push_sem(true_sem)
            
            case T_rule.FALSE.value:
                false_sem: T_attrib = T_attrib(
                    T_nont.FALSE_, 
                    FALSE(
                        type = self.bool_,
                        val = False
                    )
                )
                self.push_sem(false_sem)
            
            case T_rule.CHR.value:
                chr: T_attrib = T_attrib(
                    T_nont.CHR_, 
                    CHR(
                        type = self.char_,
                        pos = self.current_token.secondary_token,
                        val = self.lexer.symbol_table.get_constant(self.current_token.secondary_token)
                    )
                )
                self.push_sem(chr)
            
            case T_rule.STR.value:
                str: T_attrib = T_attrib(
                    T_nont.STR_, 
                    STR(
                        type = self.string_,
                        pos = self.current_token.secondary_token,
                        val = self.lexer.symbol_table.get_constant(self.current_token.secondary_token)
                    )
                )
                self.push_sem(str)
            
            case T_rule.NUM.value:
                num: T_attrib = T_attrib(
                    T_nont.NUM_, 
                    NUM(
                        type = self.int_,
                        pos = self.current_token.secondary_token,
                        val = int(self.lexer.symbol_table.get_constant(self.current_token.secondary_token))
                    )
                )
                self.push_sem(num)
        
        
    def is_type_kind(self, p: Object) -> bool:
        return p.eKind in [T_kind.SCALAR_TYPE_, T_kind.ARRAY_TYPE_, T_kind.STRUCT_TYPE_, T_kind.ALIAS_TYPE_]
    
    
    def check_types(self, t1: Object, t2: Object) -> bool:
        if t1 == t2:
            return True
        elif t1 == self.universal_ or t2 == self.universal_:
            return True
        elif t1.eKind == T_kind.UNIVERSAL_ or t2.eKind == T_kind.UNIVERSAL_:
            return True
        elif t1.eKind == t2.eKind:
            match t1.eKind:
                case T_kind.ARRAY_TYPE_:
                    if t1.kind_info.pElemType == t2.kind_info.pElemType:
                        return self.check_types(t1.kind_info.pElemType, t2.kind_info.pElemType)

                case T_kind.STRUCT_TYPE_:
                    f1 = t1.kind_info.pFields
                    f2 = t2.kind_info.pFields
                    
                    while f1 is not None and f2 is not None:
                        if not self.check_types(f1.kind_info.pType, f2.kind_info.pType):
                            return False
                        f1 = f1.pNext
                        f2 = f2.pNext
                    
                    return f1 is None and f2 is None
                
                case T_kind.ALIAS_TYPE_:
                    return self.check_types(t1.kind_info.pBaseType, t2.kind_info.pBaseType)
        
        return False