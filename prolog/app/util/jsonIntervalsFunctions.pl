:-module('jsonIntervalsFunctions', [
    getIntervalJSON/3,
    readIntervalJSON/1,
    date_string_to_timestamp/2,
    intervaloToJSON/3,
    intervalosToJSON/2,
    deleteIntervaloJSON/3,
    writeIntervalJSON/1
    ]).
:- use_module(library(http/json)).

readIntervalJSON(File) :-
	nb_getval(dbPathInterval, Path),
	open(Path, read, F),
	json_read_dict(F, File),
	close(F). 

writeIntervalJSON(JSON) :-
    nb_getval(dbPathInterval, Path),
    open(Path, write, Stream), write(Stream, JSON), close(Stream).

getIntervalJSON([], _, _).
getIntervalJSON([H|T], H.phase, H.days).
getIntervalJSON([H|T], PhaseName, Out) :- getIntervalJSON(T, PhaseName, Out).

date_string_to_timestamp(DataString, Timestamp) :-
    split_string(DataString, "-", "", Partes),
    [DiaString, MesString, AnoString] = Partes,
    number_string(Dia, DiaString),
    number_string(Mes, MesString),
    number_string(Ano, AnoString),
    Timestamp is ((Ano - 1970) * 31536000 + ((Mes - 1) * 2592000) + Dia * 86400 + (13 * 86400) + 10800).

intervaloToJSON(Phase, Days, Out) :-
    swritef(Out, '{"phase":%w,"days":%q}', [Phase, Days]),
    writeln(Out).
    
intervalosToJSON([], []).
intervalosToJSON([H|T], [X|Out]) :- 
    intervaloToJSON(H.phase, H.days, X), 
    intervalosToJSON(T, Out).

deleteIntervaloJSON([], _, []).
deleteIntervaloJSON([H|T], H.phase, T).
deleteIntervaloJSON([H|T], Phase, [H|Out]) :- deleteIntervaloJSON(T, Phase, Out).