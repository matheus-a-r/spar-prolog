:-module('InterfaceController', [
  putLine/0,
  initialMenu/0,
  errorMenu/0
]).

welcome:-
    writeln("*** Bem-vindo ao Spar! ***\n")
    writeln("O Spar é uma ferramenta que visa ajudar estudantes a adotar a prática da repetição espaçada.\n")

putLine:-
    writeln("-------------------------------------------------\n").

initialMenu:-
    writeln("Digite a letra correspondente à ação que você deseja executar\n"),
    writeln("[E]studar\n"),
    writeln("[C]riar Pilha\n"),
    writeln("[G]erenciar Pilha\n"),
    writeln("[V]isualizar sessões de estudo anteriores\n"),
    writeln("[A]lterar intervalos\n"),
    putLine.

errorMenu:-
    writeln("################# Opção inválida! #################\n").
