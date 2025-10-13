# Compilador SSL (Simple Scripting Language)

Este repositório contém um compilador completo para a linguagem SSL (Simple Scripting Language), uma linguagem de programação similar ao C, desenvolvido em Python. O compilador realiza análise léxica, sintática e semântica, gerando código intermediário para execução.

---

## Sobre o Projeto

Este é um compilador completo que implementa todas as fases tradicionais de compilação:

- **Análise Léxica**: Converte o código fonte em tokens
- **Análise Sintática**: Verifica a estrutura gramatical usando análise LR
- **Análise Semântica**: Verifica tipos, escopo e regras de negócio
- **Geração de Código**: Produz código intermediário executável

---

## Como Usar o Compilador

### Pré-requisitos

- Python 3.7+
- Dependências listadas em `requirements.txt`

### Instalação

1. Clone o repositório:
```bash
git clone <repository-url>
cd compilador-ime
```

2. Instale as dependências:
```bash
pip install -r requirements.txt
```

### Compilando um Arquivo SSL

Para compilar um arquivo SSL, use o script principal:

```bash
python ssl_compiler.py <arquivo.txt>
```

**Exemplo:**
```bash
python ssl_compiler.py test_simples.txt
```

O compilador irá:
1. Analisar o código fonte
2. Verificar erros léxicos, sintáticos e semânticos
3. Gerar código intermediário no arquivo `output_ssl_compiled.txt`

## Sintaxe da Linguagem SSL

### Tipos de Dados
- `integer`: Números inteiros
- `char`: Caracteres
- `boolean`: Valores lógicos (true/false)
- `string`: Sequências de caracteres

### Declaração de Variáveis
```ssl
var x: integer;
var nome: string;
var ativo: boolean;
```

### Declaração de Funções
```ssl
function nomeFuncao(param1: tipo, param2: tipo): tipoRetorno
{
    // corpo da função
    return valor;
}
```

### Estruturas de Controle

**Estrutura While:**
```ssl
while(condição)
{
    // código
}
```

**Estrutura If:**
```ssl
if(condição)
{
    // código
}
else
{
    // código
}
```

### Operadores
- Aritméticos: `+`, `-`, `*`, `/`
- Comparação: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Lógicos: `&&`, `||`, `!`
- Incremento/Decremento: `++`, `--`

### Exemplo Completo
```ssl
function fatorial(n: integer): integer
{
    var i, f: integer;
    i = 2;
    f = 1;

    while(i <= n)
    {
        f = f * i++;
    }

    return f;
}
```

---

## Estrutura do Projeto

```
compilador-ime/
├── lexical.py              # Analisador léxico
├── syntatical.py           # Analisador sintático
├── ssl_compiler.py         # Script principal do compilador
├── tokens.py               # Definições de tokens
├── t_attrib.py             # Atributos semânticos
├── scope_manager.py        # Gerenciador de escopo
└── arquivos_teste/         # Arquivos de exemplo (.txt, .ssl)
```

---

## Arquivos de Teste

O projeto inclui vários arquivos de exemplo para testar diferentes funcionalidades:

- `test_simples.txt` - Exemplo básico
- `test_loop.txt` - Estruturas de loop
- `test_nested_loops.txt` - Loops aninhados
- `fatorial_soma.txt` - Função fatorial

---

## Funcionalidades do Compilador

### Análise Léxica
- Reconhece palavras-chave, identificadores, literais
- Constrói tabela de símbolos
- Reporta caracteres não reconhecidos

### Análise Sintática
- Implementa análise LR(1)
- Usa tabela de ação e goto
- Reporta erros de sintaxe

### Análise Semântica
- Verificação de tipos
- Gerenciamento de escopo
- Análise de declarações e uso de variáveis

### Geração de Código
- Produz código intermediário
- Instruções como `LOAD_CONST`, `STORE_REF`, `ADD`, etc.
- Gerenciamento de labels e jumps

---

## Saída do Compilador

Após a compilação bem-sucedida, o arquivo `output_ssl_compiled.txt` conterá o código intermediário gerado, como:

```
BEGIN_FUNC 0 1 4
    LOAD_REF 1
    LOAD_CONST 0
    STORE_REF 1
    ...
END_FUNC
```

---

## Tratamento de Erros

O compilador reporta diferentes tipos de erros:

- **Erros Léxicos**: Caracteres não reconhecidos
- **Erros Sintáticos**: Estrutura gramatical inválida
- **Erros Semânticos**: Tipos incompatíveis, variáveis não declaradas

---

## Licença

Este projeto é para fins educacionais e de aprendizado.
