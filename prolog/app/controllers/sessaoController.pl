:-module('sessaoController', [
    finalizarSessao/3
    ]).
:-use_module('../util/JsonSessoesFunctions.pl').

finalizarSessao(Data, Tempo, QtdCartoes):-
    readSessaoJSON(Sessoes),
    sessaoToJSON(Data, Tempo, QtdCartoes, OutSessao),
    sessoesToJSON(Sessoes, OutIntervalosJSON),
    append(OutIntervalosJSON, [OutSessao], OutJSON),
    writeSessaoJSON(OutJSON).

