---
title: Modelos computacionais
author: Paulo
---

# Modelo Von Neumann & Arquiteturas computacionais modernas

O modelo/arquitetura Von Neumann é um modelo computacional que descreve os componentes básicos para a computação. Criado em meados dos anos '40, esse foi um modelo bastante influencial.

Aqui está um diagrama descritor do modelo:

![Diagrama Von Neumann](/images/von-n1.png)

De maneira mais descritiva o modelo descreve 3 componentes básicos da computação:

- Entrada, ou dados que são inseridos para processamento

- Processamento

- Saída, informações que é resultado do processamento da entrada 

Sendo que o componente "Processamento" é dividido em 2 sub-compoentes

- Processador, aquilo que executa instruções geralmente passo-a-passo

- Memória, aquilo que armazena os dados processados durante o tempo de execução

Novamente temos agora o componente "Processador" dividido em 2 ou 3 sub-componentes

- Unidade de Controle, aquele que mantém o estado do que a aplicação está executando no momento

- Unidade de aritmética/lógica, a unidade a unidade que faz cálculos aritméticos

- Registradores, alguns autores referem-se os registradores sendo um terceiro sub-componente. Registradores nada mais são que memória extra só que de um nível de acesso mais rápido.

Hoje em dia a maioria senão todos os <i>hardwares</i> de computação modernos contém como base o modelo Von Neumann.

![Até o nintendinho usa como base o modelo Von Neumann!](/images/nes-arch.png)

# Modelo de Turing & Assembly

Alan Turing definiu em sua tese uma máquina abstrata em que é definido o que e não é computável. O que essa máquina consegue calcular tendo memória infinita é considerado <u>computável</u>, caso contrário é <u>não computável</u>.

Essa máquina de turing opera em uma fita infinita em que ela é dividida em várias células. Cada célula contém uma símbolo de um conjunto finito de símbolos chamado "alfabeto da máquina". A máquina de turing tem uma agulha que fica em cima dessa essa fita. Cada passo de execução essa máquina ler o símbolo da célula que essa agulha está sobreposta, Então baseado em seu estado atual e célula lida, a máquina então pode: 

- Escrever um símbolo na célula atual e mover essa agulha para esquerda ou direita.

- Simplesmente só mover essa agulha para esquerda ou direita.

- Ou parar a execução desse programa.

As linguagens da família assembly são um exemplo de máquina de turing! Onde os símbolos são os <i>opcodes</i>, o alfabeto é o <i>instruction set</i>, a agulha é o <i>stack pointer</i> e o estado atual é a memória.

# Cálculo lambda

Cálculo lambda é um outro modelo computacional, criado também para descrever o que e não é computável -- mesma finalidade do modelo de Turing -- e coincidentemente criado pelo instrutor de doutorado do Turing, Alonso Church.

Esse na minha opinião é o modelo mais abstrato e matemático comparado entre os 2 já mencionados e por natureza um pouco mais complicado de entender. Eu também não tenho uma qualificação ideal para explicar esses modelo então recomendo bastante a série sobre programação funcional da UFABC, que é a melhor que já encontrei no youtube discutindo sobre o assunto: [link da série](https://www.youtube.com/watch?v=U7gdzYiFJKg&list=PLYItvall0TqJ25sVTLcMhxsE0Hci58mpQ).