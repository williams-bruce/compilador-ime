from __future__ import annotations
from typing import Optional
from dataclasses import dataclass
from object import Object, Var, Param, Field, Function, Array, Struct, Alias
from t_kind import T_kind


@dataclass
class Object:
    nName: Optional[int] = None
    pNext: Optional[Object] = None
    eKind: T_kind = None
    kind_info: Optional[Var | Param | Field | Function | Array | Struct | Alias] = None


@dataclass
class Var:
    pType: Optional[Object] = None
    

@dataclass
class Param:
    pType: Optional[Object] = None
    

@dataclass
class Field:
    pType: Optional[Object] = None
    

@dataclass
class Function:
    pRetType: Optional[Object] = None
    pParams: Optional[Object] = None
    

@dataclass
class Array:
    pElemType: Optional[Object] = None
    nNumElens: Optional[int] = None


@dataclass
class Struct:
    pFields: Optional[Object] = None
    

@dataclass
class Alias:
    pBaseType: Optional[Object] = None
        