import csv

def test_csv():
    with open("lr_goto_and_action_items.csv", "r") as file:
        reader = csv.reader(file)
        
        list_reader = list(reader)
        
        print(list_reader[0])
        
        
def test_get_len_rules():
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
                
    
    
if __name__ == "__main__":
    test_csv()
    # lenn, left = test_get_len_rules()
    # print(lenn)
    # print(len(lenn))
    # print(left)
    # print(len(left))

    