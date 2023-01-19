:-module('mainController', [
  mainMenu/0
]).
:-use_module('InterfaceController.pl').
:-use_module('../util/JsonFunctions.pl').
:-use_module(library(readutil)).
:- set_prolog_flag('encoding', 'utf8').
:- style_check(-singleton).

mainMenu:- 
    readJSON(Pilhas),
    maplist(getPilhaName, Pilhas, PilhaNames),
    length(PilhaNames, L),
    (
      L == 0 -> MenuPilhas = 'Voc\u00EA n\u00E3o possui pilhas';
      listPilhasNamesAndIndex(1, PilhaNames, IndexedNames),
      atomic_list_concat(IndexedNames, "\n", PilhasList),
      atomic_concat("Seus pilhas:\n\n", PilhasList, MenuPilhas)
      
    ),
    writeln(MenuPilhas),
    write("\n   [E]Estudar    [C] Criar pilha  [G] Gerenciar pilha  [V] Vizualizar Sessoes de Estudo   [A]Alterar intervalos            \n"),
    write("\n> O que voce deseja? "),
    readLine(Option),
    string_upper(Option, OptionUpper),
    menuOptionsPilha(OptionUpper).

menuOptionsPilha("C") :- createPilhaMenu(), !.
menuOptionsPilha("G") :- choosePilhaMenu(), !.

createPilhaMenu():-
  write("\nDigite o nome do novo pilha: "),
  readLine(NamePilha),
  createPilha(NamePilha, []),
  putLine(),
  mainMenu().

choosePilhaMenu():-
  write("\n> Escolha o n\u00FAmero do pilha: "),
  readLine(NumPilha),
  choosePilha(NumPilha), !.

choosePilha(NumPilhaStr):-
  number_string(NumPilha, NumPilhaStr),
  readJSON(Pilhas), length(Pilhas, LenPilhas),
  NumPilha > 0, NumPilha =< LenPilhas,
  Indice is NumPilha - 1, nth0(Indice, Pilhas, Pilha),
  string_concat("\n<<  ", Pilha.name, ParcialString),
  string_concat(ParcialString, "  >>", StringName),
  writeln(StringName),
  printCardsPilha(Pilha.name),
  writeln("\n    [A] Add carta   [E] Editar carta  [R] Remover pilha   [X] Voltar\n"),
  writeln("> O que voce deseja? "),
  readLine(Option),
  string_upper(Option, OptionUpper),
  menuOptionsChoosedPilha(OptionUpper, Pilha), !.

menuOptionsChoosedPilha("A", Pilha) :- addCartaoMenu(Pilha), !.

addCartaoMenu(Pilha):-
  writeln("\n> Qual sera a frente da carta?"),
  readLine(Front),
  writeln("\n> Qual sera o verso da carta?"),
  readLine(Back),
  Cartao = [Front, Back],
  addCartao(Pilha.name, Cartao),
  writeln("\nCarta adicionada com sucesso!\n"),
  putLine(),
  mainMenu().

getPilhaName(E, Out):-
  Out = E.name.

listPilhasNamesAndIndex(_, [], []).
listPilhasNamesAndIndex(L, [H|T], [HOut|Rest]):-
  atomic_list_concat([L, " - ", H], HOut),
  L2 is L+1,
  listPilhasNamesAndIndex(L2, T, Rest).

readLine(R):- read_line_to_codes(user_input,Cs), atom_codes(A, Cs), atomic_list_concat(L, ' ', A), atom_string(A, R).

printCardsPilha(PilhaName):-
  readJSON(Pilhas),
  printCartoesPilhaJSON(Pilhas, PilhaName, Out),
  enumCartoes(Out, 1).

enumCartoes([],_).
enumCartoes([[H|T]|[]], N):-
  atomic_list_concat([N, " - ", H], HOut),
  writeln(HOut).
enumCartoes([[H|T]|R], N):-
  atomic_list_concat([N, " - ", H], HOut),
  writeln(HOut),
  N2 is N+1,
  enumCartoes(R,N2).

