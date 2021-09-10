# Projeto 1 - Haskell

## Contaminação em uma rede de pessoas

Vamos assumir que temos uma rede de interação de pessoas, representada por um grafo conexo não direcionado com pesos. Cada pessoa é um vértice no grafo e o peso da aresta entre o vértice A e B é a frequência, ou intensidade de conexão, entre A e B. Por exemplo, assuma que esse peso mede o número de vezes no mês que A e B conversam.

Se um membro do grafo é contaminado por um vírus, queremos determinar em quanto tempo todos os vértice do grafo estarão contaminados. O tempo de contaminação entre A e B é o inverso da frequência de contato entre A e B (se A e B se falam 4 vezes for mes, o tempo de contaminação entre A e B é 1/4 ou 0.25 de um mês.

### Algoritmo

O problema trata-se de encontrar o menor caminho de um vértice para todos os demais vértices de um grafo, conhecido como shortest-path tree. A solução é o algoritmo de Dijkstra, o qual considera um conjunto S de menores caminhos, iniciado com um vértice fonte I. A cada passo do algoritmo busca-se nas adjacências dos vértices pertencentes a S aquele vértice com menor distância relativa a I e adiciona-o a S e, então, repetindo os passos até que todos os vértices alcançáveis por I estejam em S.

Entretanto, veja que para esse problema específico não precisamos como resultado o caminho do vértice origem a todos os outros nós, mas apenas o tempo para a contaminação.

### Formato dos dados

#### Input

O programa deverá ler, do standard input, um arquivo no seguinte formato:

```
antonio beto 5.4
antonio denise 1.2
fabio edite  9.3
...
fabio zelia 4.5

antonio
```

As linhas do tipo "u v w" indicam que da pessoa "u" para a pessoa "v" a frequencia de contato é "w" (vezes por mês). Após todas as linhas do tipo acima, há uma linha em branco, e por fim uma linha com o nome do primeiro contaminado.

#### Output

A saída deve ser no formato, no standard output:

```
3.14
```

ou, seja, um número com 2 casas decimais que indica o tempo mínimo para que todos sejam contaminados.

### Execução

O programa será executado da seguinte forma:

```sh
runhaskell contaminacao.hs < arquivo.in 
```

Os arquivos de entrada seguirão o padrão descrito anteriormente, e há alguns exemplos no diretório *project1-haskell/testes/* (arquivos *tXX.in*), com a respectiva saída esperada (arquivos *tXX.out*).

### Especificações

Não é necessário utilizar estruturas de dados complexas como um “priority queue” que são O(1) para achar o mínimo. Pode-se fazer uma busca linear para achar o mínimo e usar as funções já disponíveis no Haskell. 

Não é permitido o uso de nenhum pacote do Haskell que não as bibliotecas padrão.
