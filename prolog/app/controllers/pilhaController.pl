:-module('pilhaController', [
    createPilha/2,
    deletePilha/1,
    editPilhaName/2,
    shuffleCards/1,
    showPilhas/0
  ]).
  :-use_module('../util/JsonFunctions.pl').
  
  % Descrição:
  %		Cria uma pilha e a salva.
  % Parâmetros:
  %		1: +Name: O nome da pilha a ser criada
  %		2: +Cards: A lista de cards iniciais
  createPilha(Name, Cards) :-
      jsonfunctions:readJSON(File),
      pilhaExists(Name, Exists),
      (
          Exists == "yes" -> 
              writeln("\nO nome de pilha fornecido ja esta em uso!"),
              writeln("A pilha nao foi criada.")
              ;
              pilhasToJSON(File, PilhasListJSON),
              pilhaToJSON(Name, Cards, PilhaJSON),
              append(PilhasListJSON, [PilhaJSON], OutJSON),
              writeJSON(OutJSON),
              writeln("\nPilha criada com sucesso!\n")
          ).
          
  % Descrição:
  %		Remove uma pilha do banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome da pilha a ser removida
  deletePilha(PilhaName) :-
      readJSON(File),
      deletePilhaJSON(File, PilhaName, Out),
      pilhasToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  
  % Descrição:
  %		Edita uma pilha no banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome da pilha a ser alterada
  %		2: +NewName: O novo nome da pilha
  editPilhaName(PilhaName, NewName) :-
      readJSON(File),
      updatePilhaNameJSON(File, PilhaName, NewName, Out),
      pilhasToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  % Descrição:
  %		Aleatoriza a ordem das cartas de uma pilha no banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome da pilha a ser aleatorizado
  shuffleCards(PilhaName) :-
      readJSON(File),
      shuffleCardsJSON(File, PilhaName, Out),
      pilhasToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  
  % Descrição:
  %		Premissa auxiliar para imprimir as pilhas na tela.
  % Parâmetros:
  %		1: +Pilhas: A lista de pilhas
  showPilhasAux([]).
  showPilhasAux([H|T]) :- 
      write("Nome:"), writeln(H.name),
      write("Cartoes:"), writeln(H.cards), nl, showPilhasAux(T).
  
  % Descrição:
  %		Imprime as pilhas na tela.
  % Parâmetros: Nenhum parâmetro
  showPilhas() :-
      readJSON(Pilhas),
      showPilhasAux(Pilhas).
  
  % Descrição:
  %		Verifica se um nome de pilha já está sendo usado no banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome da pilha a ser alterado
  %		2: -Exists: "yes" se a pilha existir, "no" se não.
  pilhaExists(PilhaName, Exists):-
    readJSON(Pilhas),
    pilhaExistsJSON(Pilhas, PilhaName, Exists).