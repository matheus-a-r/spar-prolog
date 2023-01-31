:-module('jsonIntervalsFunctions', [
    getIntervalJSON/3,
    readIntervalJSON/1,
    date_string_to_timestamp/2
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

date_string_to_timestamp(DataString, Timestamp) :-
    split_string(DataString, "-", "", Partes),
    [DiaString, MesString, AnoString] = Partes,
    number_string(Dia, DiaString),
    number_string(Mes, MesString),
    number_string(Ano, AnoString),
    Timestamp is ((Ano - 1970) * 31536000 + Mes * 2592000 + Dia * 86400).