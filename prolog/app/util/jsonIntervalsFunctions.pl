:-module('jsonIntervalsFunctions', [
    getIntervalJSON/3,
    readIntervalJSON/1
    ]).
:- use_module(library(http/json)).

readIntervalJSON(File) :-
	nb_getval(dbPathInterval, Path),
	open(Path, read, F),
	json_read_dict(F, File),
	close(F). 

getIntervalJSON([], _, _).
getIntervalJSON([H|T], H.phase, H.days).
getIntervalJSON([H|T], PhaseName, Out) :- getIntervalJSON(T, PhaseName, Out).
   