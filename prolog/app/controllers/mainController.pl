:-module('mainController', [
  mainMenu/0
]).
?- use_module(library(date)).

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
      atomic_concat("Suas pilhas:\n\n", PilhasList, MenuPilhas)
      
    ),
    writeln(MenuPilhas),
    write("\n   [E]studar    [C]riar pilha  [G]erenciar pilha  [V]isualizar Sessoes de Estudo   [A]lterar intervalos            \n"),
    write("\n> O que voce deseja? "),
    readLine(Option),
    string_upper(Option, OptionUpper),
    menuOptionsPilha(OptionUpper).

menuOptionsPilha("C") :- createPilhaMenu(), !.
menuOptionsPilha("G") :- choosePilhaMenu(), !.

createPilhaMenu():-
  write("\nDigite o nome da pilha: "),
  readLine(NamePilha),
  createPilha(NamePilha, []),
  putLine(),
  mainMenu().

choosePilhaMenu():-
  write("\n> Escolha o n\u00FAmero da pilha: "),
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
  writeln("\n    [A]dicionar cartão   [E]studar pilha  [R]emover pilha   [V]oltar\n"),
  writeln("> O que voce deseja? "),
  readLine(Option),
  string_upper(Option, OptionUpper),
  menuOptionsChoosedPilha(OptionUpper, Pilha), !.

menuOptionsChoosedPilha("A", Pilha) :- addCartaoMenu(Pilha), !.
menuOptionsChoosedPilha("R", Pilha) :- removePilha(Pilha), !.
menuOptionsChoosedPilha("E", Pilha) :- estudarPilha(Pilha, Pilha.cards), !.
menuOptionsChoosedPilha("V", Pilha) :- mainMenu, !.

estudarPilha(Pilha, []) :- mainMenu().
estudarPilha(Pilha, [Cartao|Cartoes]) :-
  nth0(0, Cartao, Frente),
  nth0(1, Cartao, Verso),
  writeln(Frente),
  writeln("\n Pressione ENTER para ver a resposta <\n"),
  get_single_char(_),
  writeln(Verso),
  writeln("\n Pressione ENTER para ver o próximo cartão <\n"),
  get_single_char(_),
  estudarPilha(Pilha, Cartoes).

addCartaoMenu(Pilha):-
  writeln("\n> Qual sera a frente do cartão?"),
  readLine(Front),
  writeln("\n> Qual sera o verso do cartão?"),
  readLine(Back),
  get_time(TimeStamp),
  %stamp_date_time(TimeStamp, DateTime, 'UTC'),
  %date_time_value(date, DateTime, Value),
  format_time(string(DateTimeString), '%d-%m-%Y', TimeStamp),
  Cartao = [Front, Back, DateTimeString, DateTimeString , "0"],
  addCartao(Pilha.name, Cartao),
  writeln("\nCartão adicionado com sucesso!\n"),
  putLine(),
  mainMenu().

removePilha(Pilha):-
  writeln("\n> Tem certeza que deseja remover a pilha? [Y]"),
  readLine(Option),
  string_upper(Option, OptionUpper),
  confirmRemove(OptionUpper, Pilha).

confirmRemove("Y", Pilha):- 
  deletePilha(Pilha.name), 
  writeln("\nA pilha foi removida com sucesso!\n"),
  putLine(), mainMenu(), !.

editCardMenu(Pilha):-
  writeln("Escolha o cartão que deseja editar: "),
  readLine(Option),
  atom_number(Option, Number),
  Number2 is Number - 1,
  findCard(Pilha, Pilha.cards, Number2, Aux),
  editCard(Pilha, Aux),
  removeCartao(Pilha.name, Aux),
  mainMenu().
    
editCard(Pilha, [_, _, D, V, F]):-
  writeln("Qual sera a nova frente do cartão? "),
  readLine(Frente),
  writeln("Qual sera o novo verso do cartão? "),
  readLine(Verso),
  NewCartao = [Frente, Verso, D, V , F],
  addCartao(Pilha.name, NewCartao).

%findCard(Pilha, [], Indice, Aux):- writeln("Cartao nao encontrado"), editCardMenu(Pilha).      
findCard(Pilha, [H|T], Indice, Aux):- NewIndice is Indice - 1, findCard(Pilha, T, NewIndice, Aux).
findCard(Pilha, [H|T], 0, Aux):- Aux = H.

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