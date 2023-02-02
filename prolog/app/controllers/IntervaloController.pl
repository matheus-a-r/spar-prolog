:-module('IntervaloController',[
    alteraIntervalo/2
]).
:-use_module('util/JsonIntervalsFunctions.pl').

alteraIntervalo(Phase, Days):-
    readIntervalJSON(Intervalos),
    intervaloToJSON(Phase, Days, IntervaloJSON),
    deleteIntervaloJSON(Intervalos, Phase, NewIntervalos),
    intervalosToJSON(NewIntervalos, OutIntervalosJSON),
    append(OutIntervalosJSON, [IntervaloJSON], OutJSON),
    writeIntervalJSON(OutJSON).