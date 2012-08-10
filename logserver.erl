%%
%%  Simple data logger.
%%

-module(logserver).
-author("Aaron France").
-export([run/0,
         goServer/0,
         goState/2,
         server/0,
         addLotsOfData/3]).

%% Starts/Restarts our services
run() ->
    checkState(),
    checkServer().

%% checks the state of the database process, restarting it if it's
%% active and starting/registering it if not.
checkState() ->
    {database, node()} ! {alive, self()},
    receive
        alive ->
            {database, node()} ! restart
    after
        10000 ->
            register(database, spawn(?MODULE, goState, [dict:new(), 0]))
    end.

%% Checks the server process, restarting it if need be.
checkServer() ->
    {server, node()} ! {alive, self()},
    receive
        alive ->
            {server, node()} ! restart
    after
        10000 ->
            spawn(?MODULE, goServer, [])
    end.    

%% Starts the state loop.
goState(State, N) ->
    receive
        {addition, Data} ->
            goState(dict:store(N, Data, State), N+1);
        restart ->
            logserver:goState(State, N);
        {alive, Pid} ->
            Pid ! alive,
            goState(State, N);
        count ->
            io:format("~p~n",[N]),
            goState(State, N);
        clear ->
            io:format("Clearing ~p entry", [N]),
            goState(dict:new(),0)
    end.

%% Executes the server and monitors it's process.
goServer() ->
    process_flag(trap_exit, true),
    register(server, spawn_link(?MODULE, server, [])),

    receive
        {'EXIT', Pid, restart } ->
            io:format("Catching restart on: ~p~n", [Pid]),
            logserver:goServer()
    end.

%% Server loop.
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


%% test methods
addLotsOfData(Data, WhereAt, 0) ->
    Data;
addLotsOfData(Data, WhereAt, N) ->
    {Level, Host, Time, D} = Data,
    {server, WhereAt} ! {newlog, Level, Host, Time, D},
    addData(Data, WhereAt, N-1).
