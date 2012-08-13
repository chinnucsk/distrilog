%%Simple data logger
-module(logserver).
-author("Aaron France").

%% Call these functions directly.
%%
%% They will asynchronously start up the server process and the
%% database process associated with it. Along with the supervisor
%% processes which watch their state.
-export([run/0 ]).
% Exported methods which shouldn't be called directly.
-export([goServer/0,goState/1,server/0]).
%% Usability methods.
-export([addLotsOfData/3,codeswitch/1,codeswitch/0,
         quit/0,quit/1,count/0]).
-include("dbfuncs.erl").


%% Starts/Restarts our services
run() ->
    io:format("~p~n",[init_table(database_log, [node()])]),
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
        1 ->
            register(database, spawn(?MODULE, goState, [0]))
    end.

%% Checks the server process, restarting it if need be.
checkServer() ->
    {server, node()} ! {alive, self()},
    receive
        alive ->
            {server, node()} ! restart
    after
        1 ->
            spawn(?MODULE, goServer, [])
    end.    

%% Starts the state loop.
goState(N) ->
    receive
        {addition, Data} ->
            io:format("~p~n",[add_entry(Data)]),
            goState(N);
        {alive, Pid} ->
            Pid ! alive,
            goState(N);
        restart ->
            logserver:goState(N);
        quit ->
            ok
    end.

%% Returns the highest ID in the database in use.
count() ->
    {idproc, node()} ! {count, self()},
    receive
        N ->
            N
    after
        1 ->
            error
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
        {newlog, Data} ->
            {database, node()} ! {addition, Data},
            server();
        {getlog, Return, Id} ->
            RVal = find_by_id(Id),
            Return ! RVal,
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

%% Switches the code out
codeswitch() ->
    {database, node()} ! restart,
    {server, node()} ! restart,
    {idproc, node()} ! {restart, self()}.
codeswitch(Node) ->
    {database, Node} ! restart,
    {server, Node} ! restart.

%% Quits the application
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
    {server, WhereAt} ! {newlog, Data},
    addLotsOfData(Data, WhereAt, N-1).
