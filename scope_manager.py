


class Object:
    nName = None
    pNext = None


class ScopeManager:
    def __init__(self):
        self.symbol_table: list[Object | None] = []
        self.symbol_table_last: list[Object | None] = []
        self.current_level: int = -1


    def new_block(self):
        self.current_level += 1
        self.symbol_table[self.current_level] = None
        self.symbol_table_last[self.current_level] = None
        return self.current_level
    
    
    def end_block(self):
        self.current_level -= 1
        return self.current_level


    def define(self, aName: int) -> Object:
        obj: Object | None = Object(aName, None)
        
        if self.symbol_table[self.current_level] is None:
            self.symbol_table[self.current_level] = obj
            self.symbol_table_last[self.current_level] = obj
        
        else:
            self.symbol_table_last[self.current_level].pNext = obj
            self.symbol_table_last[self.current_level] = obj

        return obj


    def search(self, aName: int) -> Object | None:
        obj: Object | None = self.symbol_table[self.current_level]
        
        while obj is not None:
            if obj.nName == aName:
                break
            obj = obj.pNext
        
        return obj


    def find(self, aName: int) -> Object | None:
        obj: Object | None = None
        i: int = self.current_level
        
        while i >= 0:
            obj = self.symbol_table[i]
            while obj is not None:
                if obj.nName == aName:
                    break
                obj = obj.pNext
                
            if obj is not None:
                break
            i -= 1
        
        return obj