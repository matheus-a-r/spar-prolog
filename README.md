# Spar

## Introdução

Uma das maneiras mais eficazes de aprender um assunto novo é através da repetição. Ao [rever o conteúdo repetidamente](https://www.theguardian.com/education/2016/jan/23/spaced-repetition-a-hack-to-make-your-brain-store-information) em intervalos variados de tempo, conseguimos guardar conteúdo na nossa memória de longo prazo e criar conexões cada vez mais fortes entre os conceitos. Adotar essa prática também possibilita eliminar o [cramming](https://web.archive.org/web/20071009104548/http://www.bmb.psu.edu/courses/psu16/troyan/studyskills/cramming.htm), que consiste em tentar absorver (normalmente sem sucesso) grandes quantidades de informação em pequenos períodos.


O Spar é uma ferramenta que visa ajudar estudantes a adotar a prática da repetição espaçada. Sua funcionalidade gira em torno de cartões, que possuem frente e verso contendo, respectivamente, perguntas e respostas. O estudante vê a frente do cartão, responde mentalmente e confere o verso do cartão para verificar se acertou. O Spar coletará feedback do estudante (se ele acertou ou errou) e irá, com base nessa informação, distribuir os cartões estudados ao longo dos dias. Todo dia, ao abrir o Spar, o estudante terá acesso ao que deve estudar.
## Recursos

- Estudar os cartões que vencem no dia atual.
- Criar e editar pilhas.
- Criar e editar cartões.
- Definir intervalos customizados, ou seja, quantos dias depois o usuário quer estudar novamente uma carta que foi acertada 1, 2, 3, 4 ou 5 vezes.
- Ver as sessões de estudos, com o tempo que cada sessão durou e a quantidade de cartas que foram estudadas.

## Instalação e uso
Esta versão do Spar foi desenvolvida e testada na implementação do Prolog [SWI-Prolog](https://www.swi-prolog.org/download/stable). Recomendamos o uso de uma versão estável dele para executar o Spar.

Após a instalação do SWI-Prolog, no diretório raiz do Spar, execute o comando `swipl -q -f app/main.pl`. Ao carregar o programa, digite `main.` e pressione Enter para carregar a interface do Spar.

## Capturas de tela
![Menu inicial do Spar](https://i.ibb.co/k0kxhBw/Captura-de-Tela-2023-02-03-s-10-19-04.png)
![Estudando um cartão no Spar](https://i.ibb.co/J7X48JZ/Captura-de-Tela-2023-02-03-s-10-19-26.png)
![Gerenciando os cartões no Spar](https://i.ibb.co/yk7fJcw/Captura-de-Tela-2023-02-03-s-10-19-40.png)
![Visualizando as sessões de estudo já realizadas no Spar](https://i.ibb.co/DR9nZHT/Captura-de-Tela-2023-02-03-s-11-47-33.png)
![Alterando os intervalos no Spar](https://i.ibb.co/GHCg3GJ/Captura-de-Tela-2023-02-03-s-10-20-10.png)