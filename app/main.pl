:- use_module('controllers/InterfaceController.pl').

main():-
    welcome(),
	initialMenu(),
	putLine()
    mainMenu(),
    halt. 