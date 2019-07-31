-module(repl).

%% API exports
-export([run/0]).

%%====================================================================
%% API functions
%%====================================================================
run() ->
    read_eval_process().

%%====================================================================
%% Auxiliary functions
%%====================================================================
read_eval_process() ->
    Line = io:get_line("> "),
    Out = process_line(Line),
    io:format("< ~s~n~n", [Out]),
    read_eval_process().

process_line(Line) ->
    string:uppercase(Line).
