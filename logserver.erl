-module(logserver).
-author("Aaron France").
-export([run/0,
         goServer/0,
         goState/2,
         server/0,
         addData/2]).

run() ->
    checkState(),
    checkServer().

checkState() ->
    {database, node()} ! {alive, self()},
    receive
        alive ->
            {database, node()} ! restart
    after
        1000 ->
            register(database, spawn(?MODULE, goState, [orddict:new(), 0]))
    end.

checkServer() ->
    {server, node()} ! {alive, self()},
    receive
        alive ->
            {server, node()} ! restart
    after
        1000 ->
            spawn(?MODULE, goServer, [])
    end.    

goState(State, N) ->
    receive
        {addition, Data} ->
            io:format("~p~n", [N]),
            goState(orddict:store(N, Data, State), N+1);
        restart ->
            logserver:goState(State, N);
        {alive, Pid} ->
            Pid ! alive,
            goState(State, N)
    end.

goServer() ->
    process_flag(trap_exit, true),
    register(server, spawn_link(?MODULE, server, [])),

    receive
        {'EXIT', Pid, restart } ->
            io:format("Catching restart on: ~p~n", [Pid]),
            logserver:goServer()
    end.

server() ->
    receive
        {newlog, Level, Host, Time, Data} ->
            {database, node()} ! {addition, {Level, Host, Time, Data}},
            server();
        {getlog, Return, Tag} ->
            io:format("Receive: ~p~nTag:~p~n", [Return, Tag]),
            server();
        restart ->
            exit(self(), restart);
        {alive, Pid} ->
            Pid ! alive,
            server()
    after
        10000 ->
            io:format("Polling..~n"),
            server()
    end.

addData(Data, WhereAt) ->
    {Level, Host, Time, D} = Data,
    {server, WhereAt} ! {newlog, Level, Host, Time, D}.
