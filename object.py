from __future__ import annotations
from typing import Optional
from dataclasses import dataclass
from t_kind import T_kind


@dataclass
class Object:
    nName: Optional[int] = None
    pNext: Optional[Object] = None
    eKind: T_kind = None
    kind_info: Optional[Var | Param | Field | Function | Array | Struct | Alias | Type] = None


@dataclass
class Var:
    nSize: Optional[int] = None
    pType: Optional[Object] = None
    nIndex: Optional[int] = None
    

@dataclass
class Param:
    nSize: Optional[int] = None
    pType: Optional[Object] = None
    nIndex: Optional[int] = None


@dataclass
class Field:
    nSize: Optional[int] = None
    pType: Optional[Object] = None
    nIndex: Optional[int] = None


@dataclass
class Function:
    pRetType: Optional[Object] = None
    pParams: Optional[Object] = None
    nIndex: Optional[int] = None
    nParams: Optional[int] = None
    nVars: Optional[int] = None


@dataclass
class Array:
    nSize: Optional[int] = None
    pElemType: Optional[Object] = None
    nNumElens: Optional[int] = None


@dataclass
class Struct:
    nSize: Optional[int] = None
    pFields: Optional[Object] = None


@dataclass
class Alias:
    nSize: Optional[int] = None
    pBaseType: Optional[Object] = None


@dataclass
class Type:
    nSize: Optional[int] = None
    pBaseType: Optional[Object] = None