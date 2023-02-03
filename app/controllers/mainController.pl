:-module('mainController', [
  mainMenu/0,
  getIntervalo/2
]).
?- use_module(library(date)).
:- use_module('../util/jsonIntervalsFunctions.pl').
:- use_module('../util/jsonSessoesFunctions.pl').
:-use_module('InterfaceController.pl').
:-use_module('IntervaloController.pl').
:-use_module('../util/JsonFunctions.pl').
:-use_module('SessaoController.pl').
:-use_module(library(readutil)).
:- set_prolog_flag('encoding', 'utf8').
:- style_check(-singleton).

mainMenu:- 
    readJSON(Pilhas),
    readSessaoJSON(Sessoes),
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
menuOptionsPilha("A", Pilhas) :- alterarIntervalosMenu, !.
menuOptionsPilha("V", Pilhas) :- printSessoes, !.

createPilhaMenu():-
  write("\nDigite o nome da pilha: "),
  readLine(NamePilha),
  createPilha(NamePilha, []),
  putLine(),
  mainMenu().

studyPilhaMenu(Pilhas) :-
  choosePilhaMenu(Pilhas, Pilha),
  get_time(Inicio),
  random_permutation(Pilha.cards, ShuffledPilhaCards),
  filterDueToday(ShuffledPilhaCards, [], ParaEstudar),
  length(ParaEstudar, L),
  (
    L == 0 -> writeln("\nNao ha cartoes para estudar hoje nesta pilha\n"), putLine, mainMenu;
              studyPilha(Pilha, ParaEstudar, 0, NumeroCartoes),
              get_time(Fim),
              Duracao is Fim - Inicio,
              format_time(string(DateTimeString), '%d-%m-%Y', Inicio),
              finalizarSessao(DateTimeString, Duracao, NumeroCartoes),
              writeln("Sessao de estudo finalizada."),
              putLine,
              mainMenu()
  ).
  
filterDueToday([Cartao|Cartoes], Vencidos, Final) :-
  vencido(Cartao),
  append(Vencidos, [Cartao], NovoVencidos),
  filterDueToday(Cartoes, NovoVencidos, Final).

filterDueToday([Cartao|Cartoes], Vencidos, Final) :-
  \+ vencido(Cartao),
  filterDueToday(Cartoes, Vencidos, Final).

filterDueToday([], Vencidos, Final) :- Final = Vencidos.

vencido(Cartao) :-
  nth0(3, Cartao, ValidadeCartaoString),
  date_string_to_timestamp(ValidadeCartaoString, ValidadeCartao),
  get_time(Hoje),
  Hoje >= ValidadeCartao.

studyPilha(Pilha, [], Contador, Final) :- Final = Contador.
studyPilha(Pilha, [Cartao|Cartoes], Contador, Final) :-
  nth0(0, Cartao, Frente),
  nth0(1, Cartao, Verso),
  putLine,
  writeln(Frente),
  writeln("\n Pressione ENTER para ver a resposta <\n"),
  get_single_char(_),
  writeln(Verso),
  writeln("\nQual foi o nivel de dificuldade?\n[1]Dificil   [2]Mediano  [3]Facil  [X]Parar \n"),
  readLine(Option),
  string_upper(Option, OptionUpper),
  (OptionUpper == "X"
    -> NovoContador is Contador + 1, studyPilha(Pilha, [], NovoContador, Final)
    ;
    studyPilhaDifficultyOptions(Option, Pilha, Cartao),
    NovoContador is Contador + 1,
    studyPilha(Pilha, Cartoes, NovoContador, Final)
  ). 
  
studyPilhaDifficultyOptions("X",_ , _)  :- finalizarSessao, !.
studyPilhaDifficultyOptions("1", Pilha, Cartao) :- studyPilhaDifficulty(-1, Pilha, Cartao).
studyPilhaDifficultyOptions("2", Pilha, Cartao) :- studyPilhaDifficulty(0, Pilha, Cartao).
studyPilhaDifficultyOptions("3", Pilha, Cartao) :- studyPilhaDifficulty(1, Pilha, Cartao).


studyPilhaDifficulty(Incremento, Pilha, Cartao) :-
  nth0(3, Cartao, DataString),
  nth0(4, Cartao, FaseString),
  date_string_to_timestamp(DataString, DataTs),
  number_string(Fase, FaseString),
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
  writeln("\n    [A]dicionar cartao   [E]ditar cartao  [R]emover pilha   [V]oltar\n"),
  writeln("> O que voce deseja? "),
  readLine(Option),
  string_upper(Option, OptionUpper),
  menuOptionsChoosedPilha(OptionUpper, Pilha), !.

menuOptionsChoosedPilha("A", Pilha) :- addCartaoMenu(Pilha), !.
menuOptionsChoosedPilha("R", Pilha) :- removePilha(Pilha), !.
menuOptionsChoosedPilha("E", Pilha) :- editCardMenu(Pilha), !.
menuOptionsChoosedPilha("V", Pilha) :- mainMenu, !.

addCartaoMenu(Pilha):-
  writeln("\n> Qual sera a frente do cartao?"),
  readLine(Front),
  writeln("\n> Qual sera o verso do cartao?"),
  readLine(Back),
  get_time(TimeStamp),
  format_time(string(DateTimeString), '%d-%m-%Y', TimeStamp),
  Cartao = [Front, Back, DateTimeString, DateTimeString , "0"],
  addCartao(Pilha.name, Cartao),
  writeln("\nCartao adicionado com sucesso!\n"),
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
  writeln("Escolha o cartao que deseja editar: "),
  readLine(Option),
  atom_number(Option, Number),
  Number2 is Number - 1,
  findCard(Pilha, Pilha.cards, Number2, Aux),
  writeln("Qual sera a nova frente do cartao? "),
  readLine(Frente),
  writeln("Qual sera o novo verso do cartao? "),
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
          L == 0 -> MenuPilhas = 'Voce nao possui pilhas';
          listPilhasNamesAndIndex(1, PilhaNames, IndexedNames),
          atomic_list_concat(IndexedNames, "\n", PilhasList),
          putLine,
          atomic_concat("Suas pilhas:\n\n", PilhasList, MenuPilhas)

    ),
    write(MenuPilhas),
    writeln("\n"),
    putLine.

choosePilhaMenu(Pilhas, Pilha):-
  printPilhas(Pilhas),
  write("\n> Escolha o numero da pilha ou digite V pra voltar: "),
  readLine(NumPilha),
  choosePilha(NumPilha, Pilha), !.

choosePilha("V", Pilha) :- mainMenu, !.
choosePilha(NumPilhaStr, Pilha):-
  number_string(NumPilha, NumPilhaStr),
  readJSON(Pilhas), length(Pilhas, LenPilhas),
  (
    NumPilha > 0, NumPilha =< LenPilhas -> Indice is NumPilha - 1, nth0(Indice, Pilhas, Pilha);
    writeln("\nNumero da pilha e invalido\n"),
    choosePilhaMenu(Pilhas, Pilha) 
  ).
  
getIntervalo(Phase, Intervalo):-
  readIntervalJSON(Intervalos),
  getIntervalJSON(Intervalos, Phase, Intervalo).


alterarIntervalosMenu:-
  putLine,
  writeln("\n> Quantos dias depois voce quer ver uma carta acertada nenhuma vez? "),
  readLine(Option0),
  atom_number(Option0, Number0),
  alteraIntervalo(0, Number0),

  writeln("\n> Quantos dias depois voce quer ver uma carta acertada uma vez? "),
  readLine(Option1),
  atom_number(Option1, Number1),
  alteraIntervalo(1, Number1),

  writeln("\n> Quantos dias depois voce quer ver uma carta acertada duas vezes? "),
  readLine(Option2),
  atom_number(Option2, Number2),
  alteraIntervalo(2, Number2),

  writeln("\n> Quantos dias depois voce quer ver uma carta acertada tres vezes? "),
  readLine(Option3),
  atom_number(Option3, Number3),
  alteraIntervalo(3, Number3),

  writeln("\n> Quantos dias depois voce quer ver uma carta acertada quatro vezes? "),
  readLine(Option4),
  atom_number(Option4, Number4),
  alteraIntervalo(4, Number4),
  
  writeln("\n> Quantos dias depois voce quer ver uma carta acertada cinco vezes? "),
  readLine(Option5),
  atom_number(Option5, Number5),
  alteraIntervalo(5, Number5),

  writeln("\nIntervalos salvos com sucesso!\n"),
  putLine,
  mainMenu.

printSessoes:-
  readSessaoJSON(Sessoes),
  writeln("\n"),
  formataSessoes(Sessoes),
  writeln("\n"),
  putLine,
  mainMenu.

formataSessoes([]).
formataSessoes([H|T]):-
  swritef(Out, 'Data de estudo: %w, Duracao: %q, Cartoes Estudados: %d\n', [H.dataEstudo, H.duracao, H.cartoesEstudados]),
  writeln(Out),
  formataSessoes(T).
  

