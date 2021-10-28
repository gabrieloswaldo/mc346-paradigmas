# Projeto 3 - Python

## Montagem de trechos de genoma com interseção

No processamento de genoma, a metodologia é quebrar o DNA em trechos (pequenos), e sequenciar esses trechos. O problema deste projeto é montar o genoma de volta dado a sequência dos trechos.

Vamos dizer que 2 trechos tem uma interseção se o final de um deles tem os mesmos componentes do começo do outro, e essa interseção tem 4 ou mais elementos.

Assim:

```
abcdefghij
fghijaaagguujj
```

tem interseção pois o final do primeiro `fghij` é exatamente igual ao começo do segundo trecho, e o tamanho dessa parte em comum é maior ou igual a 4.

Já,

```
abcdefghij
hijxxxuuuvvv
```

não satisfazem o critério, pois a parte comum `hij` tem menos que 4 letras.

### Objetivo

Escreva um programa em Python que lê do standard input os vários trechos (um por linha), e imprime as sequências resultantes da montagem dos trechos. As sequências podem estar em qualquer ordem.

### Formato dos dados

#### Input

Você deve ler de standard input várias linhas que indicam os vários trechos, em uma ordem qualquer.

```
xxxxxababababyyyyyy
yyaaaaaaaaaaa
yyyyyyeeeeeeeeeeeeee
cccccccccccccccxxxxx
fffffffffffffffwwwwww
wwwwwwgggggggggggxx
```

O objetivo é montar as maiores sequências permitidas pelos trechos, juntando, em ordem, aqueles que tem interseção.

Note que a linha 4 tem interseção com a linha 1, que tem intercessão com a linha 3, nesta ordem. Já a linha 5 tem interseção com a linha 6, nessa ordem, e a linha 2 não tem interceção com nenhuma outra. Note que a parte comum de 2 trechos não precisa ser uma letra repetida, este exemplo foi apenas utilizado para que as partes em comum ficassem visualmente claras.

#### Output

A saída deve ser no standard output. Assim, com os trechos do exemplo acima, o máximo que podemos dizer é que o genoma é composto de 3 segmentos:

```
cccccccccccccccxxxxxababababyyyyyyeeeeeeeeeeeeee
fffffffffffffffwwwwwwgggggggggggxx
yyaaaaaaaaaaa
```

onde o primeiro segmento é a montagem da linha 4 com 1 com 3, o segundo a montagem da linha 5 com 6 a a terceira, a linha 2 apenas.

### Execução

O programa será executado da seguinte forma:

```sh
python genoma.py < arquivo.in
```

Há alguns exemplos de teste no diretório *project3-python/testes/* (arquivos *tXX.in*), com uma possível saída esperada (arquivos *tXX.out*). Lembrando que as sequências podem estar em qualquer ordem.

### Especificações

O projeto deverá rodar com qualquer python >= 3.7.

Só é permitido o uso das bibliotecas padrão.
