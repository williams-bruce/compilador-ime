from __future__ import annotations
from t_nont import T_nont
from dataclasses import dataclass
from object import Object


@dataclass
class T_attrib:
    t_nont: T_nont
    attrib: PROGRAMA | LISTA_DECLARACOES_EXTERNAS | DECLARACAO_EXTERNA | TIPO | DECLARACAO_TIPO | DECLARACAO_CAMPOS | DECLARACAO_FUNCAO | LISTA_PARAMETROS | BLOCO | LISTA_DECLARACAO_VARIAVEIS | LISTA_COMANDOS | DECLARACAO_VARIAVEL | LISTA_IDENTIFICADORES | COMANDO | EXPRESSAO | EXPRESSAO_L | EXPRESSAO_R | EXPRESSAO_Y | EXPRESSAO_F | LISTA_EXPRESSOES | VALOR_ESQUERDO | IDD | IDU | ID | TRUE | FALSE | CHR | STR | NUM


@dataclass
class PROGRAMA:
    pass


@dataclass
class LISTA_DECLARACOES_EXTERNAS:
    pass


@dataclass
class DECLARACAO_EXTERNA:
    pass


@dataclass
class TIPO:
    type: Object


@dataclass
class DECLARACAO_TIPO:
    obj: Object


@dataclass
class DECLARACAO_CAMPOS:
    list: Object


@dataclass
class DECLARACAO_FUNCAO:
    pass


@dataclass
class LISTA_PARAMETROS:
    pass


@dataclass
class BLOCO:
    pass


@dataclass
class LISTA_DECLARACAO_VARIAVEIS:
    pass


@dataclass
class LISTA_COMANDOS:
    pass


@dataclass
class DECLARACAO_VARIAVEL:
    list: Object


@dataclass
class LISTA_IDENTIFICADORES:
    list: Object


@dataclass
class COMANDO:
    pass


@dataclass
class EXPRESSAO:
    pass


@dataclass
class EXPRESSAO_L:
    pass


@dataclass
class EXPRESSAO_R:
    pass


@dataclass
class EXPRESSAO_Y:
    pass


@dataclass
class EXPRESSAO_F:
    pass


@dataclass
class LISTA_EXPRESSOES:
    pass


@dataclass
class VALOR_ESQUERDO:
    pass


@dataclass
class IDD:
    name: int
    obj: Object


@dataclass
class IDU:
    name: int
    obj: Object


@dataclass
class ID:
    name: int
    obj: Object


@dataclass
class TRUE:
    type: Object
    val: bool


@dataclass
class FALSE:
    type: Object
    val: bool


@dataclass
class CHR:
    type: Object
    pos: int
    val: str


@dataclass
class STR:
    type: Object
    pos: int
    val: str


@dataclass
class NUM:
    type: Object
    pos: int
    val: int