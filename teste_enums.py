from t_rules import T_rule
from t_nont import T_nont
from tokens import TokenType
from t_kind import T_kind


def test_t_rule():
    print("Testando valores de T_rule:")
    for rule in T_rule:
        print(f'{rule.name}: {rule.value}')
    print("\n")
        

def test_t_nont():
    print("Testando valores de T_nont:")
    for nont in T_nont:
        print(f'{nont.name}: {nont.value}')
    print("\n")
    
    
def test_token_type():
    print("Testando valores de TokenType:")
    for token_type in TokenType:
        print(f'{token_type.name}: {token_type.value}')
    print("\n")
    

def test_t_kind():
    print("Testando valores de T_kind:")
    for kind in T_kind:
        print(f'{kind.name}: {kind.value}')
    print("\n")
    
        
        
if __name__ == "__main__":
    test_t_rule()
    test_t_nont()
    test_token_type()
    test_t_kind()