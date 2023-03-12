%%%-------------------------------------------------------------------
%%% @author Makiror
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(pingpong).

-export([start/1]).

start(Times) ->
    Pid = spawn(fun() -> pong() end),
    spawn(fun() -> ping(Times, Pid) end).

pong() ->
    receive
        {ping, Times, PingPid} ->
            io:format("Pong ~p ~n", [Times]),
            PingPid ! pong,
            pong();
        stop ->
            io:format("Pong received stop~n")
    end.

ping(Pong, 0) ->
    Pong ! stop,
    io:format("Ping stop ~n");

ping(Pong, Times) ->
    Pong ! {ping, Times, self()},
    receive
        pong ->
            io:format("Ping ~p ~n", [Times]),
            ping(Pong, Times - 1 )
    end.