---
title: Lógica Condicional
author: Paulo Artur
---

Na disciplina lógica computacional, um dos primeiros assuntos estudados são os operadores lógicos, tal como: $\land$, $\lor$, $\rightarrow$, $\leftrightarrow$, $\oplus$.

Todos eles fazem total sentido para mim menos o operador condicional ($\rightarrow$). Vamos supor 2 variáveis $p$ e $q$. e a proposição $p \rightarrow q$.

A única maneira dessa expressão ser avaliado como falsa é quando $V(p) = 1$ e $V(q) = 0$. O que não fazia sentido para mim era o por quê apenas nesse contexto essa proposição era avaliado dessa maneira.

Se fomos considerar a expressão de cima como esse snippet de código, tudo faria sentido.

```python
if p == True:
    print(q)
else:
    print(True)
```

Nesse snippet tenho um statement condicional ``if-else``. Traduzindo essa expressão para portugês temos

> se $p$ for verdadeiro, então a saída vai ser o valor de $q$, se não a saída vai ser o valor verdadeiro.

## Agora vamos para os exemplos:

- ``p = True``

- ``q = False``

```python 
if True == True:
    print(False)
else:
    print(True)
```

Como True é igual a True a saída vai ser 

```
False
```

### Outro exemplo

- ``p = False``

- ``q = False``

```python 
if False == True:
    print(False)
else:
    print(True)
```

Como ``False`` é diferente de ``True``, a expressão vai cair no bloco ``else``, então a saída do programa vai ser:

```
True
```

### Outro exemplo

- ``p = True``

- ``q = True``

```python 
if True == True:
    print(True)
else:
    print(True)
```

O bloco ``if`` vai ser avaliado, saída:

```
True
```

### Último exemplo

- ``p = False``

- ``q = True``

```python 
if False == True:
    print(True)
else:
    print(True)
```

O bloco ``else`` vai ser avaliado, saída.

```
True
```
