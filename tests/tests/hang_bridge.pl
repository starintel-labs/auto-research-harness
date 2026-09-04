:- initialization(main, main).
:- use_module(library(http/json)).
main :- json_read_dict(current_input, _, [end_of_file(error)]), sleep(3600), halt(0).
