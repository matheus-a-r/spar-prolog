:-module('pilhaController', [
    createPilha/2,
    deletePilha/1,
    editPilhaName/2,
    shuffleCards/1,
    showPilhas/0
  ]).
  :-use_module('../util/JsonFunctions.pl').
  
  % Descrição:
  %		Cria um pilha e o salva.
  % Parâmetros:
  %		1: +Name: O nome do pilha a ser criado
  %		2: +Cards: A lista de cards iniciais
  createPilha(Name, Cards) :-
      jsonfunctions:readJSON(File),
      pilhaExists(Name, Exists),
      (
          Exists == "yes" -> 
              writeln("\nO nome de pilha fornecido ja esta em uso!"),
              writeln("O pilha nao foi criado.")
              ;
              pilhasToJSON(File, PilhasListJSON),
              pilhaToJSON(Name, Cards, PilhaJSON),
              append(PilhasListJSON, [PilhaJSON], OutJSON),
              writeJSON(OutJSON),
              writeln("\nPilha criado com sucesso!\n")
          ).
          
  % Descrição:
  %		Remove um pilha do banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome do pilha a ser removido
  deletePilha(PilhaName) :-
      readJSON(File),
      deletePilhaJSON(File, PilhaName, Out),
      pihasToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  
  % Descrição:
  %		Edita um pilha no banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome do pilha a ser alterado
  %		2: +NewName: O novo nome do pilha
  editPilhaName(PilhaName, NewName) :-
      readJSON(File),
      updatePilhaNameJSON(File, PilhaName, NewName, Out),
      pilhasToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  % Descrição:
  %		Aleatoriza a ordem das cartas de um pilha no banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome do pilha a ser aleatorizado
  shuffleCards(PilhaName) :-
      readJSON(File),
      shuffleCardsJSON(File, PilhaName, Out),
      pilhasToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  
  % Descrição:
  %		Premissa auxiliar para imprimir os pilhas na tela.
  % Parâmetros:
  %		1: +Pilhas: A lista de pilhas
  showPilhasAux([]).
  showPilhasAux([H|T]) :- 
      write("Name:"), writeln(H.name), 
      write("Cards:"), writeln(H.cards), nl, showPilhasAux(T).
  
  % Descrição:
  %		Imprime os pilhas na tela.
  % Parâmetros: Nenhum parâmetro
  showPilhas() :-
      readJSON(Pilhas),
      showPilhasAux(Pilhas).
  
  % Descrição:
  %		Verifica se um nome de pilha já está sendo usado no banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome do pilha a ser alterado
  %		2: -Exists: "yes" se o pilha existir, "no" se não.
  pilhaExists(PilhaName, Exists):-
    readJSON(Pilhas),
    pilhaExistsJSON(Pilhas, PilhaName, Exists).