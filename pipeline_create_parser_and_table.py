import os

def main():
    print("Creating parser and table...")
    os.system("python parser_ply.py teste_completo_compilador.txt")
    print("Done!")
    print("Exporting parser table...")
    os.system("python export_lr_action_items_to_csv.py")
    print("Done!")
    
if __name__ == "__main__":
    main()
