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

views() -> 
    Views = [#{name => 'demo/latency',
               description => "The distribution of latencies",
               tags => [method],
               measure => 'repl/latency',
               aggregation => latency_distribution()},
             #{name => 'demo/lines_in',
               description => "The number of lines from standard input",
               tags => [method],
               measure => 'repl/line_length',
               aggregation => oc_stat_aggregation_count},
             #{name => 'demo/errors',
               description => "The number of errors",
               tags => [],
               measure => 'repl/errors',
               aggregation => oc_stat_aggregation_count},
             #{name => 'demo/line_length',
               description => "Groups the length of keys in buckets",
               tags => [method],
               measure => 'repl/line_length',
               aggregation => size_distribution()}],
    [oc_stat_view:subscribe(V) || V <- Views].
