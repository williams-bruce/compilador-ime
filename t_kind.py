from enum import Enum, auto


class T_kind(Enum):
    NO_KIND_DEF_ = -1
    VAR_ = auto()
    PARAM_ = auto()
    FUNCTION_ = auto()
    FIELD_ = auto()
    ARRAY_TYPE_ = auto()
    STRUCT_TYPE_ = auto()
    ALIAS_TYPE_ = auto()
    SCALAR_TYPE_ = auto()
    UNIVERSAL_ = auto()
