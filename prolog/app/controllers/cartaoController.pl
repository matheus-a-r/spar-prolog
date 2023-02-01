:-module('cartaoController', [
    addCartao/2,
    removeCartao/2,
    proximaFase/5
  ]).
  ?- use_module(library(date)).
  :-use_module('util/JsonFunctions.pl').
  :-use_module('util/JsonIntervalsFunctions.pl').
  :-use_module('controllers/mainController.pl').
  
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

  proximaFase(FaseAtual, DataAtual, Incremento, ProximaData, ProximaFase) :-
    ProximaFase is FaseAtual + Incremento,
    getIntervalo(ProximaFase, Dias),
    ProximaData is DataAtual + (Dias * 86400),
    writeln(FaseAtual),
    writeln(DataAtual),
    writeln(Incremento),
    writeln(Dias),
    writeln(ProximaData).

  proximaFase(5, DataAtual, 1, ProximaData, ProximaFase) :-
    proximaFase(4, DataAtual, 1, ProximaData, ProximaFase).

  proximaFase(0, DataAtual, -1, ProximaData, ProximaFase) :-
    proximaFase(1, DataAtual, -1, ProximaData, ProximaFase).