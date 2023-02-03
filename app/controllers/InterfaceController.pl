:-module('InterfaceController', [
  putLine/0,
  errorMenu/0,
  welcome/0
]).

welcome:-
    writeln("*** Bem-vindo ao Spar! ***\n"),
    writeln("O Spar eh uma ferramenta que visa ajudar estudantes a adotar a pratica da repeticao espacada.\n").

putLine:-
    writeln("-------------------------------------------------\n").

errorMenu:-
    writeln("################# Opcao invalida! #################\n").
