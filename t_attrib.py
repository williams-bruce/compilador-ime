from t_nont import T_nont
from dataclasses import dataclass
from scope_manager import Object


@dataclass
class T_attrib:
    t_nont: T_nont
    attrib: any


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
    pass


@dataclass
class DECLARACAO_TIPO:
    pass


@dataclass
class DECLARACAO_CAMPOS:
    pass


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
    pass


@dataclass
class LISTA_IDENTIFICADORES:
    pass


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
    pass


@dataclass
class FALSE:
    pass


@dataclass
class CHR:
    pass


@dataclass
class STR:
    pass


@dataclass
class NUM:
    pass