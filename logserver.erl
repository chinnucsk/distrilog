-module(logserver).
-author("Aaron France").
-export([run/0,
	 goServer/0,
	 goState/2,
	 server/0]).

run() ->
    quit(server),
    quit(database),
    spawn(?MODULE, goServer, []),
    register(database, spawn(?MODULE, goState, [orddict:new(),0])).

goState(State, N) ->
    receive

	{addition, Data} ->
	    orddict:store(N, Data, State),
	    io:format("~p~n", [Data]),
	    goState(State, N+1);

	quit ->
	    io:format("Here"),
	    exit(self(), kill)
    end.

quit(ProcessName) ->
    Val = whereis(ProcessName),
    if 
	Val =/= undefined ->
	    unregister(ProcessName);
	true ->
	    ok
    end,
    {ProcessName, node()} ! quit.


goServer() ->

    process_flag(trap_exit, true),
    register(server, spawn_link(?MODULE, server, [])),

    receive
	{"EXIT", Pid, _ } ->
	    io:format("Catching crash on: ~p~n", [Pid]),
	    goServer();
	quit ->
	    exit(self(), kill)
    end.

server() ->
    receive
	{newlog, Level, Host, Time, Data} ->
	    io:format("~p\t~p\t~p\t~p\t~n", [Level, Host, Time, Data]),
	    {database, node()} ! {addition, {Level, Host, Time, Data}},
	    server();
	{getlog, Return, Tag} ->
	    io:format("Receive: ~p~nTag:~p~n", [Return, Tag]),
	    server();
	quit ->
	    io:format("Closing connection..."),
	    exit(whereis(server), kill)
    end.
