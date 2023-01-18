:-module('jsonfunctions', [
	readJSON/1,
	writeJSON/1,
	pilhaToJSON/3,
	pilhasToJSON/2,
	deletePilhaJSON/3,
	updatePilhaNameJSON/4,
	shuffleCardsJSON/3,
	addCardJSON/4,
	removeCardJSON/4,
	pilhaExistsJSON/3
]).
:- use_module(library(http/json)).

% Descrição:
%		Lê o arquivo do banco de dados salvo na variável global "dbPath".
% Parâmetros:
%		1: -File: A lista de Pilhas (todo o arquivo JSON).
readJSON(File) :-
	nb_getval(dbPath, Path),
	open(Path, read, F),
	json_read_dict(F, File),
	close(F). 

% Descrição:
%		Escreve (para atualizar) o JSON fornecido no arquivo de banco de dados.
% Parâmetros:
%		1: +JSON: A lista de Pilhas atualizada.
writeJSON(JSON) :-
	nb_getval(dbPath, Path),
	open(Path, write, Stream), write(Stream, JSON), close(Stream).

% Descrição:
%		Retorna o Pilha fornecido como um atom em formato JSON.
% Parâmetros:
%		1: +Name: O nome do pilha.
%		2: +Cards: A lista de cards (lista de atoms).
%		3: -Out: Resultado.
pilhaToJSON(Name, Cards, Out) :-
	swritef(Out, '{"name":"%w","cards":%q}', [Name, Cards]).

% Descrição:
%		Converte uma lista de pilhas para um objeto JSON (para ser usado em writeJSON/1).
% Parâmetros:
%		1: +Pilhas: A lista de Pilhas.
%		2: -Out: Saída.
pilhasToJSON([], []).
pilhasToJSON([H|T], [X|Out]) :- 
	pilhaToJSON(H.name, H.cards, X), 
	pilhasToJSON(T, Out).

% Descrição:
%		Atualiza o nome do pilha
% Parâmetros:
%		1: +Pilhas: A lista de pilhas
%		2: +PilhaName: O nome do pilha a ser alterado
%		3: +NewName: O novo nome do pilha
%		4: -Out: A nova lista de Pilhas
updatePilhaNameJSON([], _, _, []).
updatePilhaNameJSON([H|T], H.name, NewName, [_{name:NewName, cards:H.cards}|T]).
updatePilhaNameJSON([H|T], PilhaName, NewName, [H|Out]) :- 
	updatePilhaNameJSON(T, PilhaName, NewName, Out).

% Descrição:
%		Adiciona um card ao pilha fornecido
% Parâmetros:
%		1: +Pilhas: A lista de Pilhas
%		2: +PilhaName: O nome do pilha a ser alterado
%		3: +NewCard: O novo card a ser adicionado
%		4: -Out: A nova lista de Pilhas
addCardJSON([], _, _, []).
addCardJSON([H|Rest], H.name, NewCard, [_{name:H.name, cards: Cards}|RestOut]) :-
	append([NewCard], H.cards, Cards),
	addCardJSON(Rest, H.name, NewCard, RestOut).
addCardJSON([H|T], PilhaName, NewCard, [H|Out]) :- 
	addCardJSON(T, PilhaName, NewCard, Out).

% Descrição:
%		Remove um card do Pilha
% Parâmetros:
%		1: +Pilhas: A lista de Pilhas
%		2: +PilhaName: O nome do pilha a ser alterado
%		3: +CardToRemove: O card a ser removido
%		4: -Out: A nova lista de Pilhas
removeCardJSON([], _, _, []).
removeCardJSON([H|Rest], H.name, CardToRemove, [_{name:H.name, cards: Cards}|RestOut]) :-
	delete(H.cards, CardToRemove, Cards),
	removeCardJSON(Rest, H.name, CardToRemove, RestOut).
removeCardJSON([H|T], PilhaName, CardToRemove, [H|Out]) :- 
	removeCardJSON(T, PilhaName, CardToRemove, Out).

% Descrição:
%		Aleatoriza a ordem das cartas no pilha.
% Parâmetros:
% 	1: +Pilhas: Lista de Pilhas (Obtido no readJSON/1)
% 	2: +PilhaName: Nome do Pilha a ser aleatorizado
% 	3: -Out: Saída da lista de Pilhas (Para salvar no writeJSON/1)
shuffleCardsJSON([], _, _).
shuffleCardsJSON([H|Rest], PilhaName, [_{name:H.name, cards: Cards}|RestOut]) :-
	length(Cards, L),
	(H.name == PilhaName -> 
		(L > 0 -> 
			random_permutation(H.cards, Cards),
			shuffleCardsJSON(Rest, PilhaName, RestOut)
			)
			;
			Cards = H.cards,
			shuffleCardsJSON(Rest, PilhaName, RestOut)
		).
shuffleCardsJSON([H|T], PilhaName, [H|Out]) :- 
	shuffleCardsJSON(T, PilhaName, Out).

% Descrição:
%		Remove um pilha da lista de Pilhass
% Parâmetros:
%		1: +Pilhas: A lista de Pilhas
%		2: +PilhaName: O nome do pilha a ser removido
%		3: -Out: A nova lista de Pilhas
deletePilhaJSON([], _, []).
deletePilhaJSON([H|T], H.name, T).
deletePilhaJSON([H|T], PilhaName, [H|Out]) :- deletePilhaJSON(T, PilhaName, Out).

% Descrição:
%		Verifica se o pilha existe na lista de Pilhas
% Parâmetros:
%		1: +Pilhas: A lista de Pilhas
%		2: +PilhaName: O nome do pilha a ser alterado
%		3: -Exists: "yes" se o pilha existir, "no" se não.
pilhaExistsJSON([], _, "no").
pilhaExistsJSON([H|T], PilhaName, Exists):-
  (
    H.name == PilhaName -> Exists = "yes";
    pilhaExistsJSON(T, PilhaName, Exists)).