from lexical import Lexer
import csv


class SyntaticalAnalyzer:
    def __init__(self, lexer: Lexer):
        self.lexer = lexer
        self.tokens = lexer.tokenize()
        self.stack = []
        with open("lr_goto_and_action_items.csv", "r") as file:
            reader = csv.reader(file)
            self._action = list(reader)
        
        self.len_rules, self.left_side_of_rules = self._get_rules_info()


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
        a = self.next_token()
        
        while q != 1:
            print(f'pilha: {self.stack}')
            tok_name = '$end' if a.type.name == 'EOF' else a.type.name
            p = self.action(q, tok_name)
            try:
                p = int(p)
            except:
                p = 0
            
            if self.is_shift(p):
                self.push(p)
                a = self.next_token()
            elif self.is_reduce(p):
                r = self.rule(p)
                print(f"Reducing rule: {r}")
                self.pop(self.len_rules[r])
                self.push(self.action(self.top(), self.left_side_of_rules[r]))
            else:
                raise Exception(f"SytntaxError: {tok_name} in line {a.line}. Lexeme: \'{a.lexeme}\'. column: {a.col}")

            q = self.top()
        
        print("\nAnalysis completed successfully")