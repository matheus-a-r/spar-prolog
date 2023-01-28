:-module('InterfaceController', [
  putLine/0,
  errorMenu/0,
  welcome/0
]).

welcome:-
    writeln("*** Bem-vindo ao Spar! ***\n"),
    writeln("O Spar é uma ferramenta que visa ajudar estudantes a adotar a prática da repetição espaçada.\n").

putLine:-
    writeln("-------------------------------------------------\n").

errorMenu:-
    writeln("################# Opção inválida! #################\n").
