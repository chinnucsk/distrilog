%%Simple data logger

-module(logserver).
-author("Aaron France").

%% Call these functions directly.
%%
%% They will asynchronously start up the server process and the
%% database process associated with it. Along with the supervisor
%% processes which watch their state.
-export([run/0, init_table/1]).

%% Exported methods which shouldn't be called directly.
-export([goServer/0,goState/2,server/0]).

%% Usability methods.
-export([addLotsOfData/3,clear_database/1,clear_database/0,
         codeswitch/1,codeswitch/0,quit/0,quit/1]).

-record(log, {level, host, info}).

init_table(WhichTable) ->
    mnesia:create_table(WhichTable).

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
        100 ->
            register(database, spawn(?MODULE, goState, [dict:new(), 0]))
    end.

%% Checks the server process, restarting it if need be.
checkServer() ->
    {server, node()} ! {alive, self()},
    receive
        alive ->
            {server, node()} ! restart
    after
        100 ->
            spawn(?MODULE, goServer, [])
    end.    

%% Starts the state loop.
goState(State, N) ->
    receive
        {addition, Data} ->
            goState(dict:store(N, Data, State), N+1);
        {alive, Pid} ->
            Pid ! alive,
            goState(State, N);
        {count, Pid} ->
            Pid ! N,
            goState(State, N);
        clear ->
            io:format("Clearing ~p entries", [N]),
            goState(dict:new(),0);
        restart ->
            logserver:goState(State, N);
        quit ->
            ok
    end.

%% Executes the server and monitors it's process.
goServer() ->
    process_flag(trap_exit, true),
    register(server, spawn_link(?MODULE, server, [])),

    receive
        {'EXIT', Pid, restart } ->
            io:format("Catching restart on: ~p~n", [Pid]),
            logserver:goServer();
        {'EXIT', Pid, quit} ->
            io:format("Catching quit on: ~p~n", [Pid]),
            ok
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
        {alive, Pid} ->
            Pid ! alive,
            server();
        restart ->
            exit(self(), restart);
        quit ->
            exit(self(), quit)
    after
        10000 ->
            io:format("Polling..~n"),
            server()
    end.

clear_database() ->
    {database, node()} ! clear.
clear_database(Where) ->
    {database, Where} ! clear.

codeswitch() ->
    {database, node()} ! restart,
    {server, node()} ! restart.
codeswitch(Node) ->
    {database, Node} ! restart,
    {server, Node} ! restart.

quit() ->
    {database, node()} ! quit,
    {server, node()} ! quit.
quit(Node) ->
    {database, Node} ! quit,
    {server, Node} ! quit.

%% test methods
addLotsOfData(Data, _, 0) ->
    Data;
addLotsOfData(Data, WhereAt, N) ->
    {Level, Host, Time, D} = Data,
    {server, WhereAt} ! {newlog, Level, Host, Time, D},
    addLotsOfData(Data, WhereAt, N-1).
