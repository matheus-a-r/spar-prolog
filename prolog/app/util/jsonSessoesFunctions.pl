:-module('jsonSessoesFunctions',[
    readSessaoJSON/1,
    writeSessaoJSON/1,
    sessaoToJSON/4,
    sessoesToJSON/2]).

:- use_module(library(http/json)).

readSessaoJSON(File) :-
	nb_getval(dbPathSessoes, Path),
	open(Path, read, F),
	json_read_dict(F, File),
	close(F).

writeSessaoJSON(JSON) :-
    nb_getval(dbPathSessoes, Path),
    open(Path, write, Stream), write(Stream, JSON), close(Stream).

sessaoToJSON(Data, Tempo, QtdCartoes, Out) :-
    swritef(Out, '{"dataEstudo":"%w","duracao":%q, "cartoesEstudados":%d}', [Data, Tempo, QtdCartoes]).
        
sessoesToJSON([], []).
sessoesToJSON([H|T], [X|Out]) :- 
    sessaoToJSON(H.dataEstudo, H.duracao, H.cartoesEstudados, X), 
    sessoesToJSON(T, Out).
