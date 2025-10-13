from lexical import Lexer
import csv
from t_attrib import T_attrib
from t_attrib import IDD, IDU, ID, TIPO, LISTA_IDENTIFICADORES, DECLARACAO_VARIAVEL
from t_attrib import TRUE, FALSE, CHR, STR, NUM, DECLARACAO_TIPO, DECLARACAO_CAMPOS
from t_attrib import LISTA_PARAMETROS, EXPRESSAO, EXPRESSAO_L
from t_attrib import EXPRESSAO_R, EXPRESSAO_Y, EXPRESSAO_F, VALOR_ESQUERDO
from t_attrib import MARCADOR_C, LISTA_EXPRESSOES, MARCADOR_FUNCAO
from t_attrib import MT, ME, MW
from t_rules import T_rule
from scope_manager import ScopeManager
from typing import Generator, Optional
from tokens import Token
from t_nont import T_nont
from object import Object, Var, Array, Alias, Field, Struct, Param, Function
from t_kind import T_kind
import os


class SyntaticalAnalyzer:
    def __init__(self, lexer: Lexer, output_file_name: str = "output_ssl_compiled.txt"):
        self.lexer = lexer
        self.tokens: Generator[Token, None, None] = lexer.tokenize()
        self.stack: list[int] = []
        self.stack_sem: list[T_attrib] = []
        with open("lr_goto_and_action_items.csv", "r") as file:
            reader = csv.reader(file)
            self._action = list(reader)
        self.output_file = open(output_file_name, "w")
        
        self.len_rules, self.left_side_of_rules = self._get_rules_info()
        self.scope_manager = ScopeManager()
        
        self.int_ : Object = Object(nName=-1, pNext=None, eKind=T_kind.SCALAR_TYPE_)
        self.char_ : Object = Object(nName=-1, pNext=None, eKind=T_kind.SCALAR_TYPE_)
        self.bool_ : Object = Object(nName=-1, pNext=None, eKind=T_kind.SCALAR_TYPE_)
        self.string_ : Object = Object(nName=-1, pNext=None, eKind=T_kind.SCALAR_TYPE_)
        self.universal_ : Object = Object(nName=-1, pNext=None, eKind=T_kind.UNIVERSAL_)
        
        self.n_funcs_index: int = 0
        self._num_labels: int = 0
        self.current_function: Object = None
        self.current_function_level: int = -1  # Track the function's main block level
        self.error_flag: bool = False
        
        self.debug: bool = False
    
    
    def _get_new_n_funcs_index(self):
        n_funcs_index = self.n_funcs_index
        self.n_funcs_index += 1
        return n_funcs_index
    
    
    def get_n_funcs_index(self):
        return self.n_funcs_index
    
    
    def _new_label(self):
        label = self._num_labels
        self._num_labels += 1
        return label


    def push(self, item):
        if not isinstance(item, int):
            try:
                item = int(item)
                self.stack.append(item)
            except:
                print(f"Pushing item: {"vazio" if len(item) == 0 else item} to stack")
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
            rule_ = rule.strip().split(" ")
            if rule_[0] == '<empty>':
                len_rules.append(0)
            else:
                len_rules.append(len(rule_))
        
        return len_rules, left_side_of_rules
    
    
    def _find_index(self, token_type):
        for i, word in enumerate(self._action[0]):
            if word == token_type:
                return i
        
        raise Exception(f"Index not found for token: {self.current_token}")


    def action(self, q, a):
        index = self._find_index(a)
        state = self._action[q+1][index]
        if self.debug:
            print(f"Action called with q = {q} and a = {a} -> state = {state}")
        return state
    
    
    def analyze(self):
        q = 0
        self.push(0)
        self.current_token = self.next_token()
        
        while q != 1:
            if self.current_token.type.name == 'ID':
                self.current_id_token = self.current_token
            
            if self.current_token.type.name in ['NUMERAL', 'STRINGVAL', 'CHARACTER']:
                self.current_const_token = self.current_token
                
            if self.debug:
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
                if self.debug:
                    print(f"Reducing rule: {r}")
                self.pop(self.len_rules[r])
                self.push(self.action(self.top(), self.left_side_of_rules[r]))
                self.semantics(r)
            else:
                raise Exception(f"SytntaxError: {tok_name} in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")

            q = self.top()
            if self.debug:
                print('--------------------------------')
        
        # Close the output file
        self.output_file.close()
        
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
                t = T_attrib(
                    T_nont.TIPO_,
                    TIPO(type = self.int_, nSize = 1)
                )
                
                self.push_sem(t)
            
            case T_rule.TIPO_CHAR.value:
                t = T_attrib(
                    T_nont.TIPO_,
                    TIPO(type = self.char_, nSize = 1)
                )
                self.push_sem(t)
            
            case T_rule.TIPO_BOOLEAN.value:
                t = T_attrib(
                    T_nont.TIPO_,
                    TIPO(type = self.bool_, nSize = 1)
                )
                self.push_sem(t)
            
            case T_rule.TIPO_STRING.value:
                t = T_attrib(
                    T_nont.TIPO_,
                    TIPO(type = self.string_, nSize = 1)
                )
                self.push_sem(t)
            
            case T_rule.TIPO_IDU.value:
                idu: T_attrib = self.top_sem()
                self.pop_sem()
                
                t: T_attrib = T_attrib(
                    T_nont.TIPO_,
                    TIPO(type = self.universal_, nSize = 1)
                )
                
                p: Object = idu.attrib.obj
                if self.is_type_kind(p) or p.eKind == T_kind.UNIVERSAL_:
                    t.attrib.type = p
                    t.attrib.nSize = p.kind_info.nSize
                else:
                    t.attrib.type = self.universal_
                    t.attrib.nSize = 0
                    self._error(f"SemanticError: Type expected. Got {p.eKind}.")
                
                self.push_sem(t)
            
            case T_rule.DECLARACAO_TIPO_ARRAY.value:
                t = self.top_sem()
                num = self.top_sem(1)
                idd = self.top_sem(2)
                self.pop_sem(3)
                
                p = idd.attrib.obj
                p.eKind = T_kind.ARRAY_TYPE_
                p.kind_info = Array(
                    pElemType = t.attrib.type,
                    nNumElens = num.attrib.val,
                    nSize = t.attrib.nSize * num.attrib.val
                )
                
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
                p.kind_info = Struct(
                    pFields = dc.attrib.list,
                    nSize = dc.attrib.nSize
                )

                dt: T_attrib = T_attrib(
                    T_nont.DECLARACAO_TIPO_, 
                    DECLARACAO_TIPO(
                        idd.attrib.obj,
                    )
                )
                
                self.scope_manager.end_block()
                
                self.push_sem(dt)
                
            case T_rule.DECLARACAO_TIPO.value:
                t = self.top_sem()
                idd = self.top_sem(1)
                self.pop_sem(2)
                
                p = idd.attrib.obj
                p.eKind = T_kind.ALIAS_TYPE_
                p.kind_info = Alias(
                    pBaseType = t.attrib.type,
                    nSize = t.attrib.nSize
                )
                
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
                nnn:int = dc1.attrib.nSize
                
                p = li.attrib.list
                while p is not None and p.eKind == T_kind.NO_KIND_DEF_:
                    p.eKind = T_kind.FIELD_
                    p.kind_info = Field(
                        pType = t.attrib.type,
                        nIndex=nnn,
                        nSize = t.attrib.nSize
                    )
                    nnn += t.attrib.nSize
                    p = p.pNext
                
                dc0: T_attrib = T_attrib(
                    T_nont.DECLARACAO_CAMPOS_,
                    DECLARACAO_CAMPOS(
                        dc1.attrib.list,
                        nSize = nnn
                    )
                )
                
                self.push_sem(dc0)
            
            case T_rule.DECLARACAO_CAMPOS.value:
                t = self.top_sem()
                li = self.top_sem(1)
                self.pop_sem(2)
                nnn: int = 0

                p = li.attrib.list
                while p is not None and p.eKind == T_kind.NO_KIND_DEF_:
                    p.eKind = T_kind.FIELD_
                    p.kind_info = Field(
                        pType = t.attrib.type,
                        nSize = t.attrib.nSize,
                        nIndex = nnn
                    )
                    nnn += t.attrib.nSize
                    p = p.pNext

                dc: T_attrib = T_attrib(
                    T_nont.DECLARACAO_CAMPOS_, 
                    DECLARACAO_CAMPOS(
                        li.attrib.list,
                        nSize = nnn
                    )
                )
                
                self.push_sem(dc)

            case T_rule.DECLARACAO_FUNCAO.value:
                # Write END_FUNC here (after BLOCO has been reduced)
                self.output_file.write(f'END_FUNC\n')
                
                self.scope_manager.end_block()
                
                df: T_attrib = T_attrib(
                    T_nont.DECLARACAO_FUNCAO_,
                    None
                )
                
                self.push_sem(df)
            
            case T_rule.NEW_BLOCK.value:
                self.scope_manager.new_block()
                
                nb: T_attrib = T_attrib(
                    t_nont= T_nont.NEW_BLOCK_,
                    attrib=None
                )
                
                self.push_sem(nb)
            
            
            case T_rule.N_FUNCAO.value:
                idd = self.top_sem()
                
                fun = idd.attrib.obj
                fun.eKind = T_kind.FUNCTION_
                fun.kind_info = Function(
                    nParams = 0,
                    nVars = 0,
                    nIndex = self._get_new_n_funcs_index(),
                )
                
                nf: T_attrib = T_attrib(
                    T_nont.N_FUNCAO_,
                    None
                )
                
                self.scope_manager.new_block()
                
                self.push_sem(nf)
            
            
            case T_rule.MARCADOR_FUNCAO.value:
                t = self.top_sem()
                lp = self.top_sem(1)
                nf = self.top_sem(2)
                idd = self.top_sem(3)
                
                o = idd.attrib.obj
                # Preserve nIndex from N_FUNCAO
                saved_nIndex = o.kind_info.nIndex if o.kind_info else 0
                
                o.kind_info = Function(
                    pRetType = t.attrib.type,
                    pParams = lp.attrib.list,
                    nParams = lp.attrib.nSize,
                    nVars = 0,  # nVars counts only local variables (not parameters)
                    nIndex = saved_nIndex  # Preserve the index from N_FUNCAO
                )
                
                self.current_function = o
                # Save the current scope level (this is the function's block level)
                self.current_function_level = self.scope_manager.current_level
                
                self.output_file.write(
                    f'BEGIN_FUNC {o.kind_info.nIndex} {o.kind_info.nParams} {o.kind_info.nVars}\n'
                )
                
                mf: T_attrib = T_attrib(
                    T_nont.MARCADOR_FUNCAO_,
                    MARCADOR_FUNCAO(
                        offset = self.output_file.tell() - 3,
                        func_obj = o  # Store reference to function object
                    )
                )
                
                self.push_sem(mf)
            
            case T_rule.MARCADOR_C.value:
                    
                idu: T_attrib = self.top_sem()
                
                mc:T_attrib = T_attrib(
                    t_nont=T_nont.MARCADOR_C_,
                    attrib=MARCADOR_C(
                        type=None,
                        param=None,
                        err=False
                    )
                )
                
                fun: Object = idu.attrib.obj
                
                if fun.eKind != T_kind.FUNCTION_:
                    self._error(f"SemanticError: Kind not function")
                    mc.attrib.type=self.universal_
                    mc.attrib.param=None
                    mc.attrib.err=True
                    
                else:
                    mc.attrib.type=fun.kind_info.pRetType
                    mc.attrib.param=fun.kind_info.pParams
                    mc.attrib.err=False
                    
                
                self.push_sem(mc)
            
            case T_rule.LISTA_PARAMETROS_REC.value:
                t = self.top_sem()
                idd = self.top_sem(1)
                lp1 = self.top_sem(2)
                self.pop_sem(3)
                nnn: int = lp1.attrib.nSize
                p = idd.attrib.obj
                p.eKind = T_kind.PARAM_
                p.kind_info = Param(
                    pType = t.attrib.type,
                    nIndex = nnn,
                    nSize = t.attrib.nSize
                )
                
                lp0: T_attrib = T_attrib(
                    T_nont.LISTA_PARAMETROS_,
                    LISTA_PARAMETROS(
                        list=lp1.attrib.list,
                        nSize = t.attrib.nSize + nnn
                    )
                )
                
                self.push_sem(lp0)
            
            case T_rule.LISTA_PARAMETROS.value:
                t = self.top_sem()
                idd = self.top_sem(1)
                self.pop_sem(2)
                
                p = idd.attrib.obj
                p.eKind = T_kind.PARAM_
                p.kind_info = Param(
                    pType = t.attrib.type,
                    nIndex = 0,
                    nSize = t.attrib.nSize
                )
                
                lp: T_attrib = T_attrib(
                    T_nont.LISTA_PARAMETROS_,
                    LISTA_PARAMETROS(
                        list=p,
                        nSize = t.attrib.nSize
                    )
                )
                self.push_sem(lp)
            
            case T_rule.BLOCO.value:
                # bloco -> LEFT_BRACES lista_declaracao_variaveis lista_comandos RIGHT_BRACES
                # Pop lista_comandos and lista_declaracao_variaveis
                lc = self.top_sem()       # lista_comandos
                ldv = self.top_sem(1)     # lista_declaracao_variaveis
                self.pop_sem(2)
                
                # Find MARCADOR_FUNCAO to update nVars if this is a function block
                mf = None
                for i in range(len(self.stack_sem)):
                    item = self.top_sem(i)
                    if item and item.t_nont == T_nont.MARCADOR_FUNCAO_:
                        mf = item
                        break
                
                # If this is a function's block, update the nVars in BEGIN_FUNC
                # but DON'T write END_FUNC (that's done in DECLARACAO_FUNCAO)
                if mf is not None and mf.attrib is not None:
                    o = mf.attrib.func_obj
                    offset = mf.attrib.offset
                    
                    # Update nVars in the BEGIN_FUNC line
                    current = self.output_file.tell()
                    self.output_file.seek(offset, os.SEEK_SET)
                    self.output_file.write(f'{o.kind_info.nVars}')
                    self.output_file.seek(current, os.SEEK_SET)
                    
                    # Reset function level when exiting function's main block
                    self.current_function_level = -1
                
                # Push BLOCO to maintain stack consistency for COMANDO_BLOCO
                bloco_attrib = T_attrib(T_nont.BLOCO_, None)
                self.push_sem(bloco_attrib)
            
            case T_rule.LISTA_DECLARACAO_VARIAVEIS_REC.value:
                # Pop declaracao_variavel and previous lista_declaracao_variaveis
                self.pop_sem(2)
                # Push nothing - list rules don't need to maintain stack items
                ldv: T_attrib = T_attrib(T_nont.LISTA_DECLARACAO_VARIAVEIS_, None)
                self.push_sem(ldv)
            
            case T_rule.LISTA_DECLARACAO_VARIAVEIS.value:
                # Pop single declaracao_variavel
                self.pop_sem()
                ldv: T_attrib = T_attrib(T_nont.LISTA_DECLARACAO_VARIAVEIS_, None)
                self.push_sem(ldv)
            
            case T_rule.LISTA_COMANDOS_REC.value:
                # Pop comando and previous lista_comandos
                self.pop_sem(2)
                lc: T_attrib = T_attrib(T_nont.LISTA_COMANDOS_, None)
                self.push_sem(lc)
            
            case T_rule.LISTA_COMANDOS.value:
                # Pop single comando
                self.pop_sem()
                lc: T_attrib = T_attrib(T_nont.LISTA_COMANDOS_, None)
                self.push_sem(lc)
            
            case T_rule.DECLARACAO_VARIAVEL.value:
                t: T_attrib = self.top_sem()
                li: T_attrib = self.top_sem(1)
                self.pop_sem(2)
                
                # Only increment nVars for variables in the function's main block
                # (not in nested blocks like while/if)
                is_function_block = (self.scope_manager.current_level == self.current_function_level)
                
                # Variable indices start after parameters
                nnn = self.current_function.kind_info.nParams + self.current_function.kind_info.nVars
                count = 0
                p: Optional[Object] = li.attrib.list

                while p is not None and p.eKind == T_kind.NO_KIND_DEF_:
                    p.eKind = T_kind.VAR_
                    p.kind_info = Var(
                        pType = t.attrib.type,
                        nIndex = nnn,
                        nSize = t.attrib.nSize
                    )
                    nnn += t.attrib.nSize
                    count += t.attrib.nSize
                    p = p.pNext
                
                # Only update nVars if this is the function's main block
                if is_function_block:
                    self.current_function.kind_info.nVars += count
                
                dv: T_attrib = T_attrib(
                    T_nont.DECLARACAO_VARIAVEL_,
                    DECLARACAO_VARIAVEL(li.attrib.list)
                )
                
                self.push_sem(dv)
            
            case T_rule.LISTA_IDENTIFICADORES_REC.value:
                li1: T_attrib = self.top_sem(1)
                self.pop_sem(2)
                
                li0: T_attrib = T_attrib(
                    T_nont.LISTA_IDENTIFICADORES_,
                    LISTA_IDENTIFICADORES(
                        li1.attrib.list
                    )
                )

                self.push_sem(li0)
            
            case T_rule.LISTA_IDENTIFICADORES.value:
                idd: T_attrib = self.top_sem()
                self.pop_sem()
                li: T_attrib = T_attrib(
                    T_nont.LISTA_IDENTIFICADORES_,
                    LISTA_IDENTIFICADORES(idd.attrib.obj)
                )

                self.push_sem(li)
            
            case T_rule.MT.value:
                e = self.top_sem()
                l = self._new_label()
                
                if not self.check_types(e.attrib.type, self.bool_):
                    self._error(f"SemanticError: Type {e.attrib.type.eKind} is not a valid type. Expected type: BOOLEAN")
                
                
                mt: T_attrib = T_attrib(
                    T_nont.MT_,
                    MT(label=l)
                )
                
                self.output_file.write(f'\tTJMP_FW L{l}\n')
                
                self.push_sem(mt)
            
            case T_rule.ME.value:
                mt, mt_pos = self.find_in_stack(T_nont.MT_)
                
                if mt is None:
                    self._error("SemanticError: Could not find MT in stack for ME")
                    return
                
                label1 = mt.attrib.label
                label2 = self._new_label()
                
                me: T_attrib = T_attrib(
                    T_nont.ME_,
                    ME(label=label2)
                )
                
                self.output_file.write(f'\tJMP_FW L{label2}\nL{label1}:\n')
                
                self.push_sem(me)
            
            case T_rule.MW.value:
                # Generate label for loop start
                label_start = self._new_label()
                
                mw: T_attrib = T_attrib(
                    T_nont.MW_,
                    MW(label=label_start)
                )
                
                self.output_file.write(f'L{label_start}:\n')
                
                self.push_sem(mw)
                
            case T_rule.COMANDO_IF.value:
                mt, mt_pos = self.find_in_stack(T_nont.MT_)
                
                if mt is None:
                    self._error("SemanticError: Could not find MT in stack for COMANDO_IF")
                    return
                
                # Pop all items up to and including MT
                self.pop_sem(mt_pos + 1)
                label = mt.attrib.label
                
                self.output_file.write(f'L{label}:\n')
                
                s: T_attrib = T_attrib(
                    T_nont.COMANDO_,
                    None
                )
                
                self.push_sem(s)
            
            case T_rule.COMANDO_IF_ELSE.value:
                me, me_pos = self.find_in_stack(T_nont.ME_)
                
                if me is None:
                    self._error("SemanticError: Could not find ME in stack for COMANDO_IF_ELSE")
                    return
                
                # Pop all items up to and including ME
                self.pop_sem(me_pos + 1)
                
                label = me.attrib.label
                
                self.output_file.write(f'L{label}:\n')
                
                s: T_attrib = T_attrib(
                    T_nont.COMANDO_,
                    None
                )
                
                self.push_sem(s)
            
            case T_rule.COMANDO_WHILE.value:
                # Rule 37: comando -> WHILE mw LEFT_PARENTHESIS expressao RIGHT_PARENTHESIS mt comando
                # Stack should have: [..., mw, expressao, mt, comando]
                # Find MW and MT in stack
                mt, mt_pos = self.find_in_stack(T_nont.MT_)
                mw, mw_pos = self.find_in_stack(T_nont.MW_)
                
                if mw is None or mt is None:
                    self._error("SemanticError: Could not find MW or MT in stack for COMANDO_WHILE")
                    s = T_attrib(T_nont.COMANDO_, None)
                    self.push_sem(s)
                    return
                
                # Pop all items up to and including the deeper one (MW)
                self.pop_sem(max(mt_pos, mw_pos) + 1)
                
                label_start = mw.attrib.label
                label_end = mt.attrib.label
                
                # MT already wrote TJMP_FW L_end after the expression
                # Now we just need to:
                #   - Write JMP_BW to loop start
                #   - Write the end label
                self.output_file.write(f'\tJMP_BW L{label_start}\n')
                self.output_file.write(f'L{label_end}:\n')
                
                s: T_attrib = T_attrib(
                    T_nont.COMANDO_,
                    None
                )
                self.push_sem(s)
            
            case T_rule.COMANDO_DO.value:
                e = self.top_sem()
                mw, mw_pos = self.find_in_stack(T_nont.MW_)
                
                if mw is None:
                    self._error("SemanticError: Could not find MW in stack for COMANDO_DO")
                    return
                
                self.pop_sem(mw_pos + 1)
                
                label = mw.attrib.label
                t_: Object = e.attrib.type
                
                if not self.check_types(t_, self.bool_):
                    self._error(f"SemanticError: Type {t_.eKind} is not a valid type. Expected type: BOOLEAN")
                
                self.output_file.write(f'\tNOT\n\tTJMP_BW L{label}\n')
                
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
                
                if lv.t_nont != T_nont.VALOR_ESQUERDO_:
                    self._error(f"SemanticError: Expected VALOR_ESQUERDO but got {lv.t_nont}")
                    # Push dummy to maintain stack
                    s: T_attrib = T_attrib(T_nont.COMANDO_, None)
                    self.push_sem(s)
                    return
                
                t1: Object = lv.attrib.type
                t2: Object = e.attrib.type
                
                if not self.check_types(t1, t2):
                    self._error(f"SemanticError: Type mismatch. Left value type: {t1.eKind}. Expression type: {t2.eKind}")
                
                # For simple variables (VAR, PARAM), use STORE_VAR
                # For complex types (arrays, structs), use STORE_REF
                if lv.attrib.obj and (lv.attrib.obj.eKind == T_kind.VAR_ or lv.attrib.obj.eKind == T_kind.PARAM_):
                    self.output_file.write(f'\tSTORE_VAR {lv.attrib.obj.kind_info.nIndex}\n')
                else:
                    nSize = self.get_type_size(t1)
                    self.output_file.write(f'\tSTORE_REF {nSize}\n')
                
                s: T_attrib = T_attrib(
                    T_nont.COMANDO_,
                    None
                )
                
                self.push_sem(s)
            
            case T_rule.COMANDO_BREAK.value:
                pass
            
            case T_rule.COMANDO_RETURN.value:
                e = self.top_sem()
                self.pop_sem()
                
                if not self.check_types(e.attrib.type, self.current_function.kind_info.pRetType):
                    self._error(f"SemanticError: Return type mismatch")
                
                self.output_file.write(f'\tRET\n')
                
                s = T_attrib(T_nont.COMANDO_, None)
                
                self.push_sem(s)
            
            case T_rule.COMANDO_CONTINUE.value:
                pass
            
            case T_rule.EXPRESSAO_AND.value:
                l = self.top_sem()
                e1 = self.top_sem(1)
                self.pop_sem(2)
                
                if not self.check_types(e1.attrib.type, self.bool_):
                        self._error(f"SemanticError: Type {e1.attrib.type.eKind} is not a valid type. Expected type: BOOLEAN")
                
                if not self.check_types(l.attrib.type, self.bool_):
                    self._error(f"SemanticError: Type {l.attrib.type.eKind} is not a valid type. Expected type: BOOLEAN")
                
                self.output_file.write(f'\tAND\n')
                
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
                
                self.output_file.write(f'\tOR\n')
                
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
                    self._error(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind}")
                
                self.output_file.write(f'\tLT\n')
                
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
                    self._error(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind}")
                
                self.output_file.write(f'\tLE\n')
                
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
                    self._error(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind}")
                
                self.output_file.write(f'\tGT\n')
                
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
                    self._error(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind}")
                
                self.output_file.write(f'\tGE\n')
                
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
                    self._error(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind}")
                
                self.output_file.write(f'\tEQ\n')
                
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
                    self._error(f"SemanticError: Type mismatch. expressao_l type: {l1.attrib.type.eKind}. expressao_r type: {r.attrib.type.eKind}")
                
                self.output_file.write(f'\tNE\n')
                
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
                    self._error(f"SemanticError: Type mismatch. expressao_y type: {y.attrib.type.eKind}. expressao_r type: {r1.attrib.type.eKind}")
                
                if not self.check_types(r1.attrib.type, self.int_) and not self.check_types(r1.attrib.type, self.string_):
                    self._error(f"SemanticError: Invalid Type. expressao_r type: {y.attrib.type.eKind}")
                
                r0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_R_,
                    attrib=EXPRESSAO_R(type=r1.attrib.type)
                )
                
                self.output_file.write(f'\tADD\n')
                
                self.push_sem(r0)
            
            case T_rule.EXPRESSAO_R_MINUS.value:
                y: T_attrib = self.top_sem()
                r1: T_attrib = self.top_sem(1)
                self.pop_sem(2)

                if not self.check_types(r1.attrib.type, self.int_) and not self.check_types(r1.attrib.type, self.string_):
                    self._error(f"SemanticError: Invalid Type. expressao_r type: {y.attrib.type.eKind}")
                
                if not self.check_types(r1.attrib.type, y.attrib.type):
                    self._error(f"SemanticError: Type mismatch. expressao_y type: {y.attrib.type.eKind}. expressao_r type: {r1.attrib.type}")
                
                
                r0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_R_,
                    attrib=EXPRESSAO_R(type=r1.attrib.type)
                )
                
                self.output_file.write(f'\tSUB\n')
                
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
                    self._error(f"SemanticError: Type mismatch. expressao_y type: {f.attrib.type.eKind}. expressao_f type: {y1.attrib.type}")
                
                if not self.check_types(y1.attrib.type, self.int_):
                    self._error(f"SemanticError: Invalid Type. expressao_y type: {f.attrib.type.eKind}")
                
                y0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_Y_,
                    attrib=EXPRESSAO_Y(type=y1.attrib.type)
                )
                
                self.output_file.write(f'\tMUL\n')
                
                self.push_sem(y0)
            
            case T_rule.EXPRESSAO_Y_DIVIDE.value:
                f: T_attrib = self.top_sem()
                y1: T_attrib = self.top_sem(1)
                self.pop_sem(2)

                if not self.check_types(f.attrib.type, y1.attrib.type):
                    self._error(f"SemanticError: Type mismatch. expressao_y type: {f.attrib.type.eKind}. expressao_f type: {y1.attrib.type.eKind}")

                if not self.check_types(y1.attrib.type, self.int_):
                    self._error(f"SemanticError: Invalid Type. expressao_y type: {f.attrib.type.eKind}")

                y0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_Y_,
                    attrib=EXPRESSAO_Y(type=y1.attrib.type)
                )

                self.output_file.write(f'\tDIV\n')
                
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
                
                if lv.t_nont != T_nont.VALOR_ESQUERDO_:
                    self._error(f"SemanticError: Expected VALOR_ESQUERDO but got {lv.t_nont}")
                    # Push dummy para manter consistência da pilha
                    f = T_attrib(t_nont=T_nont.EXPRESSAO_F_, attrib=EXPRESSAO_F(type=self.universal_))
                    self.push_sem(f)
                    return

                f: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=lv.attrib.type)
                )
                
                # For simple variables (VAR, PARAM), use LOAD_VAR
                # For complex types (arrays, structs), use DE_REF
                if lv.attrib.obj and (lv.attrib.obj.eKind == T_kind.VAR_ or lv.attrib.obj.eKind == T_kind.PARAM_):
                    self.output_file.write(f'\tLOAD_VAR {lv.attrib.obj.kind_info.nIndex}\n')
                else:
                    nnn = self.get_type_size(lv.attrib.type)
                    self.output_file.write(f'\tDE_REF {nnn}\n')
                
                self.push_sem(f)
            
            case T_rule.EXPRESSAO_F_PLUS_PLUS_LEFT_VALUE.value:
                lv: T_attrib = self.top_sem()
                self.pop_sem()
                
                if not self.check_types(lv.attrib.type, self.int_):
                    self._error(f"SemanticError: Invalid Type. expressao_y type: {lv.attrib.type.eKind}")
                
                f: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )
                
                # ++LV: Load, increment, duplicate, store (value on stack is NEW value)
                # Pattern: LOAD_VAR n, INC, DUP, STORE_VAR n
                if lv.attrib.obj and (lv.attrib.obj.eKind == T_kind.VAR_ or lv.attrib.obj.eKind == T_kind.PARAM_):
                    idx = lv.attrib.obj.kind_info.nIndex
                    self.output_file.write(f'\tLOAD_VAR {idx}\n\tINC\n\tDUP\n\tSTORE_VAR {idx}\n')
                else:
                    self.output_file.write(f'\tDUP\n\tDUP\n\tDE_REF 1\n')
                    self.output_file.write(f'\tINC\n\tSTORE_REF 1\n\tDE_REF 1\n')
                
                self.push_sem(f)
            
            case T_rule.EXPRESSAO_F_MINUS_MINUS_LEFT_VALUE.value:
                lv: T_attrib = self.top_sem()
                self.pop_sem()
                
                if not self.check_types(lv.attrib.type, self.int_):
                    self._error(f"SemanticError: Invalid Type. expressao_y type: {lv.attrib.type.eKind}")
                
                f: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )
                
                # --LV: Load, decrement, duplicate, store (value on stack is NEW value)
                # Pattern: LOAD_VAR n, DEC, DUP, STORE_VAR n
                if lv.attrib.obj and (lv.attrib.obj.eKind == T_kind.VAR_ or lv.attrib.obj.eKind == T_kind.PARAM_):
                    idx = lv.attrib.obj.kind_info.nIndex
                    self.output_file.write(f'\tLOAD_VAR {idx}\n\tDEC\n\tDUP\n\tSTORE_VAR {idx}\n')
                else:
                    self.output_file.write(f'\tDUP\n\tDUP\n\tDE_REF 1\n')
                    self.output_file.write(f'\tDEC\n\tSTORE_REF 1\n\tDE_REF 1\n')
                
                self.push_sem(f)
            
            case T_rule.EXPRESSAO_F_LEFT_VALUE_PLUS_PLUS.value:
                lv: T_attrib = self.top_sem()
                self.pop_sem()
                
                if not self.check_types(lv.attrib.type, self.int_):
                    self._error(f"SemanticError: Invalid Type. expressao_y type: {lv.attrib.type.eKind}")
                
                f: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )
                
                # LV++: Load, duplicate, increment, store (value on stack is OLD value)
                # Pattern: LOAD_VAR n, DUP, INC, STORE_VAR n
                if lv.attrib.obj and (lv.attrib.obj.eKind == T_kind.VAR_ or lv.attrib.obj.eKind == T_kind.PARAM_):
                    idx = lv.attrib.obj.kind_info.nIndex
                    self.output_file.write(f'\tLOAD_VAR {idx}\n\tDUP\n\tINC\n\tSTORE_VAR {idx}\n')
                else:
                    self.output_file.write(f'\tDUP\n\tDUP\n\tDE_REF 1\n\tINC\n')
                    self.output_file.write(f'\tSTORE_REF 1\n\tDE_REF 1\n\tDEC\n')
                
                self.push_sem(f)
            
            case T_rule.EXPRESSAO_F_LEFT_VALUE_MINUS_MINUS.value:
                lv: T_attrib = self.top_sem()
                self.pop_sem()
                
                if not self.check_types(lv.attrib.type, self.int_):
                    self._error(f"SemanticError: Invalid Type. expressao_y type: {lv.attrib.type.eKind}")
                
                f: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )
                
                # LV--: Load, duplicate, decrement, store (value on stack is OLD value)
                # Pattern: LOAD_VAR n, DUP, DEC, STORE_VAR n
                if lv.attrib.obj and (lv.attrib.obj.eKind == T_kind.VAR_ or lv.attrib.obj.eKind == T_kind.PARAM_):
                    idx = lv.attrib.obj.kind_info.nIndex
                    self.output_file.write(f'\tLOAD_VAR {idx}\n\tDUP\n\tDEC\n\tSTORE_VAR {idx}\n')
                else:
                    self.output_file.write(f'\tDUP\n\tDUP\n\tDE_REF 1\n\tDEC\n')
                    self.output_file.write(f'\tSTORE_REF 1\n\tDE_REF 1\n\tINC\n')

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
                # Rule 66: expressao_f -> idu marcador_c LEFT_PARENTHESIS lista_expressoes RIGHT_PARENTHESIS
                # Stack should have: [..., idu, marcador_c, lista_expressoes]
                # Pop lista_expressoes, marcador_c, idu (3 items)
                
                le = self.top_sem()      # lista_expressoes
                mc = self.top_sem(1)     # marcador_c
                idu = self.top_sem(2)    # idu
                
                if idu.t_nont != T_nont.IDU_:
                    self._error(f"SemanticError: Expected IDU but got {idu.t_nont} in EXPRESSAO_F_IDU_LEFT_PARENTHESIS")
                    # Still need to pop and push to maintain stack
                    self.pop_sem(3)
                    f = T_attrib(t_nont=T_nont.EXPRESSAO_F_, attrib=EXPRESSAO_F(type=self.universal_))
                    self.push_sem(f)
                    return
                
                self.pop_sem(3)
                
                fun: Object = idu.attrib.obj
                
                # Check if it's a valid function
                if fun.eKind != T_kind.FUNCTION_ or fun.kind_info is None:
                    self._error(f"SemanticError: '{idu.attrib.name}' is not a function or not defined")
                    f = T_attrib(t_nont=T_nont.EXPRESSAO_F_, attrib=EXPRESSAO_F(type=self.universal_))
                    self.push_sem(f)
                    return
                
                f = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=mc.attrib.type)
                )
                
                # Check parameter count
                if not mc.attrib.err:
                    if mc.attrib.param is not None:
                        self._error(f"SemanticError: Function call with too few arguments")
                    
                self.output_file.write(f'\tCALL {fun.kind_info.nIndex}\n')

                self.push_sem(f)

            case T_rule.EXPRESSAO_F_MINUS.value:
                f1 = self.top_sem()
                self.pop_sem()

                if not self.check_types(f1.attrib.type, self.int_):
                    self._error(f"SemanticError: Invalid Type. expressao_f type: {f1.attrib.type.eKind}")

                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )
                
                self.output_file.write(f'\tNEG\n')

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_NOT.value:
                f1 = self.top_sem()
                self.pop_sem()

                if not self.check_types(f1.attrib.type, self.bool_):
                    self._error(f"SemanticError: Invalid Type. expressao_f type: {f1.attrib.type.eKind}")

                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.bool_)
                )
                
                self.output_file.write(f'\tNOT\n')

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_TRUE.value: 
                self.pop_sem() # desempilha TRUE
                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.bool_)
                )
                
                nnn = self.current_const_token.secondary_token
                self.output_file.write(f'\tLOAD_CONST {nnn}\n')

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_FALSE.value:
                self.pop_sem() # desempilha FALSE
                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.bool_)
                )

                nnn = self.current_const_token.secondary_token
                self.output_file.write(f'\tLOAD_CONST {nnn}\n')

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_CHARACTER.value:
                self.pop_sem() # desempilha CHARACTER
                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.char_)
                )

                nnn = self.current_const_token.secondary_token
                self.output_file.write(f'\tLOAD_CONST {nnn}\n')

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_STRINGVAL.value:
                self.pop_sem() # desempilha STRINGVAL
                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.string_)
                )

                nnn = self.current_const_token.secondary_token
                self.output_file.write(f'\tLOAD_CONST {nnn}\n')

                self.push_sem(f0)
            
            case T_rule.EXPRESSAO_F_NUMERAL.value:
                self.pop_sem() # desempilha NUMERAL
                f0: T_attrib = T_attrib(
                    t_nont=T_nont.EXPRESSAO_F_,
                    attrib=EXPRESSAO_F(type=self.int_)
                )

                nnn = self.current_const_token.secondary_token
                self.output_file.write(f'\tLOAD_CONST {nnn}\n')

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
                        self._error(f"SemanticError: LE with too many args")
                        le0.attrib.err = True
                    else:
                        if not self.check_types(p.kind_info.pType, e.attrib.type):
                            self._error(f"SemanticError: Error param type {n}")
                        le0.attrib.param = p.pNext
                        le0.attrib.n = n + 1
                    
                    
                self.push_sem(le0)
                        
            
            case T_rule.LISTA_EXPRESSOES.value:
                # Rule 75: lista_expressoes -> expressao
                # Stack should have: [..., expressao]
                # But we also need marcador_c which should be BEFORE expressao
                # Actually looking at the grammar, marcador_c is in the parent rule (rule 66)
                # So stack is: [..., marcador_c, expressao]
                # Wait, no. Let me think about this more carefully.
                
                # When we reduce lista_expressoes -> expressao:
                # - We have expressao on top
                # - We should NOT see marcador_c here, that's in the parent rule
                # But LISTA_EXPRESSOES needs to know about the function parameters
                # This is set in MARCADOR_C
                
                # Looking at the reference code, LISTA_EXPRESSOES takes MC from stack[-1]
                # This means MC is still on the stack below
                
                e: T_attrib = self.top_sem()      # expressao
                # MC should be below the expressao, let's find it
                mc = None
                for i in range(1, min(10, len(self.stack_sem))):
                    item = self.top_sem(i)
                    if item and item.t_nont == T_nont.MARCADOR_C_:
                        mc = item
                        break
                
                if mc is None:
                    self._error("SemanticError: Could not find MARCADOR_C for LISTA_EXPRESSOES")
                    self.pop_sem()
                    le = T_attrib(T_nont.LISTA_EXPRESSOES_, LISTA_EXPRESSOES(None, True, 0))
                    self.push_sem(le)
                    return
                
                self.pop_sem()  # Only pop expressao, MC stays
                
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
                        self._error(f"SemanticError: LE with too many args")
                        le.attrib.err = True
                    else:
                        if not self.check_types(p.kind_info.pType, e.attrib.type):
                            self._error(f"SemanticError: Error param type {n}")
                        le.attrib.param = p.pNext
                        le.attrib.n = n + 1
                
                self.push_sem(le)
            
            case T_rule.VALOR_ESQUERDO_DOT.value:
                id: T_attrib = self.top_sem()
                lv1 = self.top_sem(1)
                self.pop_sem(2)
                
                lv0: T_attrib = T_attrib(
                    t_nont=T_nont.VALOR_ESQUERDO_,
                    attrib=VALOR_ESQUERDO(type=None)
                )
                t_ = lv1.attrib.type
                
                if t_.eKind != T_kind.STRUCT_TYPE_:
                    if t_.eKind != T_kind.UNIVERSAL_:
                        self._error(f"SemanticError: Kind not struct")
                    lv0.attrib = VALOR_ESQUERDO(type=self.universal_)
                else:
                    p = t_.kind_info.pFields
                    while p is not None:
                        if p.nName == id.attrib.name:
                            break
                        p = p.pNext
                    if p is None:
                        self._error(f"SemanticError: Field not declared")
                        lv0.attrib.type = self.universal_
                    else:
                        lv0.attrib.type = p.kind_info.pType
                        
                        self.output_file.write(f'\tADD {p.kind_info.nIndex}\n')

                self.push_sem(lv0)
                        
            
            case T_rule.VALOR_ESQUERDO_LEFT_SQUARE.value:
                e: T_attrib = self.top_sem()
                lv1 = self.top_sem(1)
                self.pop_sem(2)

                lv0 = T_attrib(
                    t_nont=T_nont.VALOR_ESQUERDO_,
                    attrib=VALOR_ESQUERDO(type=None)
                )
                
                t_ = lv1.attrib.type
                
                if t_ == self.string_:
                    lv0.attrib.type=self.char_
                elif t_.eKind != T_kind.ARRAY_TYPE_:
                    if t_.eKind != T_kind.UNIVERSAL_:
                        self._error(f"SemanticError: Kind not array")
                        lv0.attrib.type = self.universal_
                    lv0.attrib.type = self.universal_
                else:
                    elem_type = t_.kind_info.pElemType
                    lv0.attrib.type = elem_type
                    elem_size = self.get_type_size(elem_type)
                    self.output_file.write(f'\tMUL {elem_size}\n\tADD\n')
                
                if not self.check_types(e.attrib.type, self.int_):
                    self._error(f"SemanticError: Invalid index type")
                
                self.push_sem(lv0)

            case T_rule.VALOR_ESQUERDO_IDU.value:
                idu = self.top_sem()
                self.pop_sem()
                p = idu.attrib.obj
                
                lv: T_attrib = T_attrib(
                    t_nont=T_nont.VALOR_ESQUERDO_,
                    attrib=VALOR_ESQUERDO(type=None, obj=None)
                )
                
                if p.eKind != T_kind.VAR_ and p.eKind != T_kind.PARAM_:
                    if p.eKind != T_kind.UNIVERSAL_:
                        self._error(f"SemanticError: Kind not VAR")
                    lv.attrib.type = self.universal_
                else:
                    lv.attrib.type = p.kind_info.pType
                    lv.attrib.obj = p  # Store the object for code generation
                
                self.push_sem(lv)
            
            case T_rule.IDD.value:
                name = self.current_id_token.secondary_token
                p = self.scope_manager.search(name)
                if p is not None:
                    print(f"SemanticError: Variable {name} already defined in line {self.current_id_token.line}. Lexeme: \'{self.current_id_token.lexeme}\'. column: {self.current_id_token.col}")
                else:
                    p = self.scope_manager.define(name)
                
                p.eKind = T_kind.NO_KIND_DEF_
                idd: T_attrib = T_attrib(T_nont.IDD_, IDD(name, p))
                    
                self.push_sem(idd)
                
            case T_rule.IDU.value:
                name = self.current_id_token.secondary_token
                idu: T_attrib = T_attrib(T_nont.IDU_, IDU(name, None))
                
                p = self.scope_manager.find(name)
                
                if p is None:
                    print(f"SemanticError: Variable {name} not defined in line {self.current_token.line}. Lexeme: \'{self.current_token.lexeme}\'. column: {self.current_token.col}")
                    p = self.scope_manager.define(name)
                
                idu.attrib.obj = p
                self.push_sem(idu)
            
            case T_rule.ID.value:
                name = self.current_id_token.secondary_token
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
                        pos = self.current_const_token.secondary_token,
                        val = self.lexer.symbol_table.get_constant(self.current_const_token.secondary_token)
                    )
                )
                self.push_sem(chr)
            
            case T_rule.STR.value:
                str: T_attrib = T_attrib(
                    T_nont.STR_, 
                    STR(
                        type = self.string_,
                        pos = self.current_const_token.secondary_token,
                        val = self.lexer.symbol_table.get_constant(self.current_const_token.secondary_token)
                    )
                )
                self.push_sem(str)
            
            case T_rule.NUM.value:
                num: T_attrib = T_attrib(
                    T_nont.NUM_, 
                    NUM(
                        type = self.int_,
                        pos = self.current_const_token.secondary_token,
                        val = int(self.lexer.symbol_table.get_constant(self.current_const_token.secondary_token))
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
    
    
    def print_stack_sem(self):
        print("++++++++++++++++++++++++++++++++")
        for item in self.stack_sem:
            print(f'item: {item.t_nont}')
        print("--------------------------------")
    
    
    def find_in_stack(self, t_nont_type, max_depth=20):
        """Find an item in the semantic stack by its t_nont type."""
        for i in range(min(max_depth, len(self.stack_sem))):
            item = self.top_sem(i)
            if item and item.t_nont == t_nont_type:
                return item, i
        return None, -1
        
        
    def _error(self, message: str):
        print(message)
        print(f"current token: {self.current_token}")
        print(f"current id token: {self.current_id_token}")
        print(f"current const token: {self.current_const_token}")
    
    
    def get_type_size(self, obj: Object) -> int:
        """Retorna o tamanho de um tipo, tratando tipos escalares corretamente."""
        if obj.eKind == T_kind.SCALAR_TYPE_:
            # Tipos escalares básicos têm tamanho 1
            return 1
        elif obj.kind_info is not None and hasattr(obj.kind_info, 'nSize'):
            return obj.kind_info.nSize
        else:
            return 1  # Valor padrão