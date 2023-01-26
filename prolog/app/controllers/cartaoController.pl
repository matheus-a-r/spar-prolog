:-module('cartaoController', [
    addCartao/2,
    removeCartao/2
  ]).
  :-use_module('util/JsonFunctions.pl').
  
  % Descrição:
  %		Adiciona um cartao a uma pilha no banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome da pilha a ser alterada
  %		2: +NewCard: O novo cartão a ser adicionado
  addCartao(PilhaName, NewCartao) :-
      readJSON(File),
      writeln(PilhaName),
      writeln(NewCartao),
      addCartaoJSON(File, PilhaName, NewCartao, Out),
      pilhasToJSON(Out, OutJSON),
      writeJSON(OutJSON).
  
  % Descrição:
  %		Adiciona um cartao a uma pilha no banco de dados.
  % Parâmetros:
  %		1: +PilhaName: O nome da pilha a ser alterada
  %		2: +NewCartao: O novo cartao a ser removido
  removeCartao(PilhaName, CartaoToRemove) :-
      readJSON(File),
      removeCartaoJSON(File, PilhaName, CartaoToRemove, Out),
      pilhasToJSON(Out, OutJSON),
      writeJSON(OutJSON).