:-module('jsonfunctions', [
	readJSON/1,
	writeJSON/1,
	deckToJSON/3,
	decksToJSON/2,
	deleteDeckJSON/3,
	updateDeckNameJSON/4,
	shuffleCardsJSON/3,
	addCardJSON/4,
	removeCardJSON/4,
	deckExistsJSON/3
]).
:- use_module(library(http/json)).

% Descrição:
%		Lê o arquivo do banco de dados salvo na variável global "dbPath".
% Parâmetros:
%		1: -File: A lista de Decks (todo o arquivo JSON).
readJSON(File) :-	
	nb_getval(dbPath, Path),
	open(Path, read, F),
	json_read_dict(F, File),
	close(F). 

% Descrição:
%		Escreve (para atualizar) o JSON fornecido no arquivo de banco de dados.
% Parâmetros:
%		1: +JSON: A lista de Decks atualizada.
writeJSON(JSON) :-
	nb_getval(dbPath, Path),
	open(Path, write, Stream), write(Stream, JSON), close(Stream).

% Descrição:
%		Retorna o deck fornecido como um atom em formato JSON.
% Parâmetros:
%		1: +Name: O nome do deck.
%		2: +Cards: A lista de cards (lista de atoms).
%		3: -Out: Resultado.
deckToJSON(Name, Cards, Out) :-
	swritef(Out, '{"name":"%w","cards":%q}', [Name, Cards]).

% Descrição:
%		Converte uma lista de decks para um objeto JSON (para ser usado em writeJSON/1).
% Parâmetros:
%		1: +Decks: A lista de Decks.
%		2: -Out: Saída.
decksToJSON([], []).
decksToJSON([H|T], [X|Out]) :- 
	deckToJSON(H.name, H.cards, X), 
	decksToJSON(T, Out).

% Descrição:
%		Atualiza o nome do deck
% Parâmetros:
%		1: +Decks: A lista de decks
%		2: +DeckName: O nome do deck a ser alterado
%		3: +NewName: O novo nome do deck
%		4: -Out: A nova lista de Decks
updateDeckNameJSON([], _, _, []).
updateDeckNameJSON([H|T], H.name, NewName, [_{name:NewName, cards:H.cards}|T]).
updateDeckNameJSON([H|T], DeckName, NewName, [H|Out]) :- 
	updateDeckNameJSON(T, DeckName, NewName, Out).

% Descrição:
%		Adiciona um card ao deck fornecido
% Parâmetros:
%		1: +Decks: A lista de Decks
%		2: +DeckName: O nome do deck a ser alterado
%		3: +NewCard: O novo card a ser adicionado
%		4: -Out: A nova lista de Decks
addCardJSON([], _, _, []).
addCardJSON([H|Rest], H.name, NewCard, [_{name:H.name, cards: Cards}|RestOut]) :-
	append([NewCard], H.cards, Cards),
	addCardJSON(Rest, H.name, NewCard, RestOut).
addCardJSON([H|T], DeckName, NewCard, [H|Out]) :- 
	addCardJSON(T, DeckName, NewCard, Out).

% Descrição:
%		Remove um card do deck
% Parâmetros:
%		1: +Decks: A lista de Decks
%		2: +DeckName: O nome do deck a ser alterado
%		3: +CardToRemove: O card a ser removido
%		4: -Out: A nova lista de Decks
removeCardJSON([], _, _, []).
removeCardJSON([H|Rest], H.name, CardToRemove, [_{name:H.name, cards: Cards}|RestOut]) :-
	delete(H.cards, CardToRemove, Cards),
	removeCardJSON(Rest, H.name, CardToRemove, RestOut).
removeCardJSON([H|T], DeckName, CardToRemove, [H|Out]) :- 
	removeCardJSON(T, DeckName, CardToRemove, Out).

% Descrição:
%		Aleatoriza a ordem das cartas no deck.
% Parâmetros:
% 	1: +Decks: Lista de Decks (Obtido no readJSON/1)
% 	2: +DeckName: Nome do Deck a ser aleatorizado
% 	3: -Out: Saída da lista de decks (Para salvar no writeJSON/1)
shuffleCardsJSON([], _, _).
shuffleCardsJSON([H|Rest], DeckName, [_{name:H.name, cards: Cards}|RestOut]) :-
	length(Cards, L),
	(H.name == DeckName -> 
		(L > 0 -> 
			random_permutation(H.cards, Cards),
			shuffleCardsJSON(Rest, DeckName, RestOut)
			)
			;
			Cards = H.cards,
			shuffleCardsJSON(Rest, DeckName, RestOut)
		).
shuffleCardsJSON([H|T], DeckName, [H|Out]) :- 
	shuffleCardsJSON(T, DeckName, Out).

% Descrição:
%		Remove um deck da lista de Decks
% Parâmetros:
%		1: +Decks: A lista de Decks
%		2: +DeckName: O nome do deck a ser removido
%		3: -Out: A nova lista de Decks
deleteDeckJSON([], _, []).
deleteDeckJSON([H|T], H.name, T).
deleteDeckJSON([H|T], DeckName, [H|Out]) :- deleteDeckJSON(T, DeckName, Out).

% Descrição:
%		Verifica se o deck existe na lista de Decks
% Parâmetros:
%		1: +Decks: A lista de Decks
%		2: +DeckName: O nome do deck a ser alterado
%		3: -Exists: "yes" se o deck existir, "no" se não.
deckExistsJSON([], _, "no").
deckExistsJSON([H|T], DeckName, Exists):-
  (
    H.name == DeckName -> Exists = "yes";
    deckExistsJSON(T, DeckName, Exists)).