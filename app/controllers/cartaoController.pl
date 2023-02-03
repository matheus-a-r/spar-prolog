:-module('cartaoController', [
    addCartao/2,
    removeCartao/2,
    proximaFase/5
  ]).
  ?- use_module(library(date)).
  :-use_module('app/util/JsonFunctions.pl').
  :-use_module('app/util/JsonIntervalsFunctions.pl').
  :-use_module('app/controllers/mainController.pl').
  
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

  proximaFase(0, DataAtual, 0, ProximaData, ProximaFase):-
    ProximaFase = 0, 
    ProximaData is DataAtual + (1 * 86400).  
  
  proximaFase(FaseAtual, DataAtual, 0, ProximaData, ProximaFase):- 
    ( FaseAtual > 5 ; FaseAtual < 0 -> ProximaFase = FaseAtual ; ProximaFase = FaseAtual),
    ProximaData = DataAtual.  
  
  proximaFase(FaseAtual, DataAtual, -1, ProximaData, ProximaFase):- 
    ( FaseAtual - 1 > 5 ; FaseAtual - 1 < 0 -> ProximaFase = FaseAtual ; ProximaFase is FaseAtual - 1),
    getIntervalo(FaseAtual, Dias), 
    ProximaData is DataAtual - (Dias * 86400).  
  
  proximaFase(FaseAtual, DataAtual, Incremento, ProximaData, ProximaFase) :-
    ( FaseAtual + Incremento > 5 ; FaseAtual + Incremento < 0 -> ProximaFase = FaseAtual ; ProximaFase is FaseAtual + Incremento),
    getIntervalo(ProximaFase, Dias),
    ProximaData is DataAtual + (Dias * 86400).
   