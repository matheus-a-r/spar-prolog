:-module('cardController', [
    addCard/2,
    removeCard/2
  ]).
  :-use_module('../util/JsonFunctions.pl').
  
  % Descrição:
  %		Adiciona uma carta a um deck no banco de dados.
  % Parâmetros:
  %		1: +DeckName: O nome do deck a ser alterado
  %		2: +NewCard: A nova carta a ser adicionada
  addCard(DeckName, NewCard) :-
      readJSON(File),
      addCardJSON(File, DeckName, NewCard, Out),
      decksToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  
  % Descrição:
  %		Adiciona uma carta a um deck no banco de dados.
  % Parâmetros:
  %		1: +DeckName: O nome do deck a ser alterado
  %		2: +NewCard: A nova carta a ser removida
  removeCard(DeckName, CardToRemove) :-
      readJSON(File),
      removeCardJSON(File, DeckName, CardToRemove, Out),
      decksToJSON(Out, OutJSON),
      writeJSON(OutJSON).