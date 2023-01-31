:-module('mainController', [
  mainMenu/0
]).
?- use_module(library(date)).
:- use_module('../util/jsonIntervalsFunctions.pl').
:-use_module('InterfaceController.pl').
:-use_module('../util/JsonFunctions.pl').
:-use_module(library(readutil)).
:- set_prolog_flag('encoding', 'utf8').
:- style_check(-singleton).

mainMenu:- 
    readJSON(Pilhas),
    write("[E]studar
    \n[C]riar pilha
    \n[G]erenciar pilha
    \n[V]isualizar Sessoes de Estudo
    \n[A]lterar intervalos            \n"),
    write("\n> O que voce deseja fazer? "),
    readLine(Option),
    string_upper(Option, OptionUpper),
    menuOptionsPilha(OptionUpper, Pilhas).

menuOptionsPilha("C", Pilhas) :- createPilhaMenu(), !.
menuOptionsPilha("G", Pilhas) :- managePilhaMenu(Pilhas), !.
menuOptionsPilha("E", Pilhas) :- studyPilhaMenu(Pilhas), !.

createPilhaMenu():-
  write("\nDigite o nome da pilha: "),
  readLine(NamePilha),
  createPilha(NamePilha, []),
  putLine(),
  mainMenu().

studyPilhaMenu(Pilhas) :-
  choosePilhaMenu(Pilhas, Pilha),
  get_time(Inicio),
  studyPilha(Pilha, Pilha.cards, 0, NumeroCartoes),
  get_time(Fim),
  Duracao is Fim - Inicio,
  writeln(NumeroCartoes),
  writeln(Duracao),
  mainMenu().

studyPilha(Pilha, [], Contador, Final) :- Final = Contador.
studyPilha(Pilha, [Cartao|Cartoes], Contador, Final) :-
  nth0(0, Cartao, Frente),
  nth0(1, Cartao, Verso),
  writeln(Frente),
  writeln("\n Pressione ENTER para ver a resposta <\n"),
  get_single_char(_),
  writeln(Verso),
  writeln("\nQual foi o nível de dificuldade?\n[1]Difícil   [2]Mediano  [3]Fácil  [X]Parar \n"),
  readLine(Option),
  studyPilhaDifficultyOptions(Option, Pilha, Cartao),
  NovoContador is Contador + 1,
  studyPilha(Pilha, Cartoes, NovoContador, Final).

studyPilhaDifficultyOptions("1", Pilha, Cartao) :- studyPilhaDifficulty(-1, Pilha, Cartao).
studyPilhaDifficultyOptions("2", Pilha, Cartao) :- studyPilhaDifficulty(0, Pilha, Cartao).
studyPilhaDifficultyOptions("3", Pilha, Cartao) :- studyPilhaDifficulty(1, Pilha, Cartao).

studyPilhaDifficulty(Incremento, Pilha, Cartao) :-
  nth0(3, Cartao, DataString),
  nth0(4, Cartao, FaseString),
  date_string_to_timestamp(DataString, DataTs),
  number_string(Fase, FaseString),
  ProxFase is Fase + Incremento,
  proximaFase(Fase, DataTs, Incremento, ProxData, ProxFase),
  format_time(string(ProxDataString), '%d-%m-%Y', ProxData),
  number_string(ProxFase, ProxFaseString),
  nth0(0, Cartao, Frente),
  nth0(1, Cartao, Verso),
  nth0(2, Cartao, Criacao),
  editCard(Pilha, Cartao, Frente, Verso, Criacao, ProxDataString, ProxFaseString).

managePilhaMenu(Pilhas) :-
  choosePilhaMenu(Pilhas, Pilha),
  string_concat("\n<<  ", Pilha.name, ParcialString),
  string_concat(ParcialString, "  >>", StringName),
  writeln(StringName),
  printCardsPilha(Pilha.name),
  writeln("\n    [A]dicionar cartão   [E]ditar cartão  [R]emover pilha   [V]oltar\n"),
  writeln("> O que voce deseja? "),
  readLine(Option),
  string_upper(Option, OptionUpper),
  menuOptionsChoosedPilha(OptionUpper, Pilha), !.

menuOptionsChoosedPilha("A", Pilha) :- addCartaoMenu(Pilha), !.
menuOptionsChoosedPilha("R", Pilha) :- removePilha(Pilha), !.
menuOptionsChoosedPilha("E", Pilha) :- editCardMenu(Pilha), !.
menuOptionsChoosedPilha("V", Pilha) :- mainMenu, !.

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
  writeln("Qual sera a nova frente do cartão? "),
  readLine(Frente),
  writeln("Qual sera o novo verso do cartão? "),
  readLine(Verso),
  nth0(2, Aux, Data),
  nth0(3, Aux, Validade),
  nth0(4, Aux, Fase),
  editCard(Pilha, Aux, Frente, Verso, Data, Validade, Fase),
  mainMenu().

editCard(Pilha, Cartao, Frente, Verso, Data, Validade, Fase) :-
  removeCartao(Pilha.name, Cartao),
  NewCartao = [Frente, Verso, Data, Validade, Fase],
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

printPilhas(Pilhas) :-
    maplist(getPilhaName, Pilhas, PilhaNames),
    length(PilhaNames, L),
    (
          L == 0 -> MenuPilhas = 'Voc\u00EA n\u00E3o possui pilhas';
          listPilhasNamesAndIndex(1, PilhaNames, IndexedNames),
          atomic_list_concat(IndexedNames, "\n", PilhasList),
          atomic_concat("Suas pilhas:\n\n", PilhasList, MenuPilhas)

    ),
    write(MenuPilhas),
    writeln("\n"),
    putLine.

choosePilhaMenu(Pilhas, Pilha):-
  printPilhas(Pilhas),
  write("\n> Escolha o n\u00FAmero da pilha ou digite V pra voltar: "),
  readLine(NumPilha),
  choosePilha(NumPilha, Pilha), !.

choosePilha("V", Pilha) :- mainMenu, !.
choosePilha(NumPilhaStr, Pilha):-
  number_string(NumPilha, NumPilhaStr),
  readJSON(Pilhas), length(Pilhas, LenPilhas),
  NumPilha > 0, NumPilha =< LenPilhas,
  Indice is NumPilha - 1, nth0(Indice, Pilhas, Pilha).

getIntervalo(Phase):-
  readIntervalJSON(Intervalos),
  getIntervalJSON(Intervalos, Phase, Intervalo),
  writeln(Intervalo).