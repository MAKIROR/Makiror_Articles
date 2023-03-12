%%%-------------------------------------------------------------------
%%% @author Makiror
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_server).
-behaviour(gen_server).

% API
-export([
    start_link/3,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    port :: integer(),
    manager :: pid()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, ValidatorName, ServerName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, ValidatorName, ServerName], []).

init([Port, ValidatorName, ServerName]) ->
    case gen_tcp:listen(Port, [binary, {packet, 2}, {active, true}]) of
        {ok, Listener} ->
            net_kernel:start([ServerName]),
            case net_adm:ping(ValidatorName) of
                pong ->
                    io:format("Chat server started: ~p~n", [self()]),
                    Manager = spawn(fun() -> manage_clients([]) end),
                    spawn(fun() -> acceptor(Listener, ValidatorName, Manager) end),
                    {ok, #state{port = Port, manager = Manager}};
                pang ->
                    io:format("Unable connect to validator~n", []),
                    {stop, "Unable connect to validator"}
            end,
            {ok, []};
        {error, Reason} ->
            io:format("Unable to start server: ~s~n", [Reason]),
            {stop, Reason}
    end.

manage_clients(Clients) ->
    receive
        {broadcast, Msg} ->
            lists:foreach(fun(Pid) -> Pid ! {msg, Msg} end, Clients),
            manage_clients(Clients);
        {add_client, Pid} ->
            manage_clients(lists:append([Pid], Clients));
        {remove_client, Pid} ->
            UpdatedClients = lists:delete(Pid, Clients),
            io:format("Client removed: ~p~n", [Pid]),
            manage_clients(UpdatedClients);
        close_all ->
            lists:foreach(fun(Pid) -> Pid ! stop end, Clients),
            manage_clients([])
    end.

acceptor(Listener, ValidatorName, Manager) -> 
    {ok, Socket} = gen_tcp:accept(Listener),
    {ok, {Address, Port}} = inet:peername(Socket),
    io:format("New connection: ~s:~p ~n",[inet:ntoa(Address), Port]),
    spawn(fun() -> acceptor(Listener, ValidatorName, Manager) end),

    receive
        {tcp, Socket, Username} ->
            case rpc:call(ValidatorName, rores_validator, verify, [binary_to_list(Username)]) of
                {ok, User} ->
                    io:format("Verification passed: ~s:~p~n",[inet:ntoa(Address), Port]),
                    Manager ! {add_client, self()},
                    Manager ! {msg, io_lib:format(" ~s joined chat room", [User])},
                    send_msg(Socket, {ok, User}),
                    receive_msg(Socket, User, Manager);
                {error, Reason} ->
                    io:format("Verification failed: ~p~n", [Reason]),
                    send_msg(Socket, {auth_failed, Reason});
                {badrpc, Reason} ->
                    case net_adm:ping(ValidatorName) of
                        pong ->
                            send_msg(Socket, {try_again});
                        pang ->
                            io:format("Unable connect to validator: ~s~n", [Reason]),
                            send_msg(Socket, {server_error})
                    end
            end;
        {error, closed} ->
            io:format("Client closed the connection~n"),
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format("Error receiving message: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

receive_msg(Socket, Username, Manager) ->
    receive
        {msg, Msg} ->
            send_msg(Socket, {msg, Msg}),
            receive_msg(Socket, Username, Manager);
        stop ->
            io:format("Close a client connection ~n"),
            gen_tcp:close(Socket);
        {tcp, Socket} ->
            receive_msg(Socket, Username, Manager);
        {tcp_closed, Socket} ->
            io:format("Close a client connection ~n"),
            gen_tcp:close(Socket),
            Manager ! {remove_client, self()};
        {tcp, Socket, Msg} ->
            Str = io_lib:format("~s: ~s ~n",[Username, Msg]),
            Manager ! {broadcast, Str},
            receive_msg(Socket, Username, Manager)
    end.

send_msg(Socket, Msg) ->
    BinData = term_to_binary(Msg),
    gen_tcp:send(Socket, BinData).

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Port = State#state.port,
    Manager = State#state.manager,
    ok = gen_tcp:close(Port),
    Manager ! close_all,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.