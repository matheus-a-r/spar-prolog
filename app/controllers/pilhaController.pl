:-module('pilhaController', [
    createDeck/2,
    deleteDeck/1,
    editDeckName/2,
    shuffleCards/1,
    showDecks/0
  ]).
  :-use_module('../util/JsonFunctions.pl').
  
  % Descrição:
  %		Cria um deck e o salva.
  % Parâmetros:
  %		1: +Name: O nome do deck a ser criado
  %		2: +Cards: A lista de cards iniciais
  createDeck(Name, Cards) :-
      jsonfunctions:readJSON(File),
      deckExists(Name, Exists),
      (
          Exists == "yes" -> 
              writeln("\nO nome de deck fornecido ja esta em uso!"),
              writeln("O deck nao foi criado.")
              ;
              decksToJSON(File, DecksListJSON),
              deckToJSON(Name, Cards, DeckJSON),
              append(DecksListJSON, [DeckJSON], OutJSON),
              writeJSON(OutJSON),
              writeln("\nDeck criado com sucesso!\n")
          ).
          
  % Descrição:
  %		Remove um deck do banco de dados.
  % Parâmetros:
  %		1: +DeckName: O nome do deck a ser removido
  deleteDeck(DeckName) :-
      readJSON(File),
      deleteDeckJSON(File, DeckName, Out),
      decksToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  
  % Descrição:
  %		Edita um deck no banco de dados.
  % Parâmetros:
  %		1: +DeckName: O nome do deck a ser alterado
  %		2: +NewName: O novo nome do deck
  editDeckName(DeckName, NewName) :-
      readJSON(File),
      updateDeckNameJSON(File, DeckName, NewName, Out),
      decksToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  % Descrição:
  %		Aleatoriza a ordem das cartas de um deck no banco de dados.
  % Parâmetros:
  %		1: +DeckName: O nome do deck a ser aleatorizado
  shuffleCards(DeckName) :-
      readJSON(File),
      shuffleCardsJSON(File, DeckName, Out),
      decksToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  
  % Descrição:
  %		Premissa auxiliar para imprimir os decks na tela.
  % Parâmetros:
  %		1: +Decks: A lista de decks
  showDecksAux([]).
  showDecksAux([H|T]) :- 
      write("Name:"), writeln(H.name), 
      write("Cards:"), writeln(H.cards), nl, showDecksAux(T).
  
  % Descrição:
  %		Imprime os decks na tela.
  % Parâmetros: Nenhum parâmetro
  showDecks() :-
      readJSON(Decks),
      showDecksAux(Decks).
  
  % Descrição:
  %		Verifica se um nome de deck já está sendo usado no banco de dados.
  % Parâmetros:
  %		1: +DeckName: O nome do deck a ser alterado
  %		2: -Exists: "yes" se o deck existir, "no" se não.
  deckExists(DeckName, Exists):-
    readJSON(Decks),
    deckExistsJSON(Decks, DeckName, Exists).