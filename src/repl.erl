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
    Start = erlang:monotonic_time(),    
    Upper = string:uppercase(Line),
    ocp:record('repl/latency', 
                erlang:convert_time_unit(erlang:monotonic_time() - Start,
                                                native, millisecond)),
    ocp:record('repl/line_length', erlang:iolist_size(Line)),
    Upper.


measures() ->
    oc_stat_measure:new('repl/latency', "The latency in millisecs per REPL loop", millisecond),
    oc_stat_measure:new('repl/errors', "The number of errors encountered", none),
    oc_stat_measure:new('repl/line_lengths', "The distribution of line lengths", bytes).