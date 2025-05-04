# YAML Parser em Ruby

Este projeto implementa um parser completo em Ruby capaz de ler mapeamentos YAML linha a linha, reconhecer e avaliar expressões aritméticas delimitadas por `$…$`, validar a sintaxe com o algoritmo de Earley e construir uma estrutura Ruby.

## Funcionalidades

- **Tokenização**  
  Separação de texto em tokens (`NUMBER`, `STRING`, `BOOL`, `BAREWORD`, símbolos aritméticos, etc.) usando `StringScanner` e expressões regulares.

- **Validação Sintática**  
  Implementação do algoritmo de Earley para qualquer gramática livre de contexto, garantindo que a entrada obedeça às regras antes de montar a estrutura de dados.

- **Parsing de Cálculos**  
  Parsing recursivo-descendente de expressões aritméticas com precedência correta (`+`, `-`, `*`, `/`, `^`) e geração de ASTs de cada operação.

- **Construção de Objeto Ruby**  
  Montagem final de um Hash Ruby a partir do parse tree, incluindo avaliação imediata de cálculos e conversão de literais em tipos nativos (Integer, Float, String, Boolean, nil).

### Gramática Suportada
```yaml
document    → entry_list
entry_list  → entry rest
rest        → NEWLINE entry rest | NEWLINE rest | ε
entry       → BAREWORD ':' value
value       → CALC | SCALAR
CALC        → '$' expr '$'
SCALAR      → NUMBER | BOOL | STRING | BAREWORD
expr        → term expr_tail
expr_tail   → (‘+’|‘–’) term expr_tail | ε
term        → factor term_tail
term_tail   → (‘*’|‘/’) factor term_tail | ε
factor      → primary factor_tail
factor_tail → '^' factor | ε
primary     → NUMBER | '(' expr ')'

```

### Entrada: 
```yaml
nome: João
idade: 30
ativo: true
conta: $9/3+21^1*10$
```

### Saída:
```yaml
{
  "nome": "João",
  "idade": 30,
  "ativo": true,
  "conta": 213
}

AST: [["divisao", 9, 3], ["potencia", 21, 1], ["soma", 3, 21], ["multiplicacao", 24, 10]]
```
