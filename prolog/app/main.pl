:- use_module('controllers/InterfaceController.pl').
:- use_module('controllers/mainController.pl').
:- use_module('controllers/pilhaController.pl').
:- use_module('controllers/cartaoController.pl').


:- working_directory(CWD, '../'),
    atom_concat('.', '/database', Path),
	absolute_file_name(Path, AbsPath),
	atom_concat(AbsPath, '/database.json', Out),
  nb_setval(dbPath, Out).

:- set_prolog_flag('encoding', 'utf8').

main():-
    welcome(),
	putLine(),
    mainMenu(). 