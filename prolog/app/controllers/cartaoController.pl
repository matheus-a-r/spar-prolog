:-module('cartaoController', [
    addCartao/2,
    removeCartao/2
  ]).
  :-use_module('../util/JsonFunctions.pl').
  
  % Descrição:
  %		Adiciona uma carta a um pilha no banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome do pilha a ser alterado
  %		2: +NewCard: A nova carta a ser adicionada
  addCartao(PilhaName, NewCartao) :-
      readJSON(File),
      addCartaoJSON(File, PilhaName, NewCartao, Out),
      pilhasToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  
  % Descrição:
  %		Adiciona uma carta a um pilha no banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome do pilha a ser alterado
  %		2: +NewCartao: A nova carta a ser removida
  removeCartao(PilhaName, CartaoToRemove) :-
      readJSON(File),
      removeCartaoJSON(File, PilhaName, CartaoToRemove, Out),
      pilhasToJSON(Out, OutJSON),
      writeJSON(OutJSON).