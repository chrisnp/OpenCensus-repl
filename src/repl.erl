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
    ocp:with_tags(#{method => "repl"}),

    Line = io:get_line("> "),
    Out = process_line(Line),
    io:format("< ~s~n~n", [Out]),
    read_eval_process().

read_line() ->
    io:get_line("> ").

process_line(Line) ->
    string:uppercase(Line).

measures() ->
    oc_stat_measure:new('repl/latency', "The latency in millisecs per REPL loop", millisecond),
    oc_stat_measure:new('repl/errors', "The number of errors encountered", none),
    oc_stat_measure:new('repl/line_lengths', "The distribution of line lengths", bytes).