-module(test).

-export([run/0]).

run() ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 9527, [binary, {packet, 2}, {active, true}]),
    gen_tcp:send(Socket, "MAKIROR"),
    receive
        {tcp, Socket, BinData} ->
            case binary_to_term(BinData) of
                {ok, Username} ->
                    io:format("Hi! ~s~n", [Username]),
                    loop(Socket);
                {auth_failed, Reason} ->
                    io:format("Verification failed: ~p~n", [Reason])
            end
    end.

loop(Socket) ->
    receive
        {tcp, Socket, BinData} ->
            case binary_to_term(BinData) of
                {close} ->
                    gen_tcp:close(Socket);
                {msg, Msg} ->
                    io:format("~ts~n",[Msg]),
                    loop(Socket)
            end
    end.
