%%Simple data logger
-module(logserver).
-author("Aaron France").
-behaviour(gen_server).

%% gen_server interface functions
-export([init/1,handle_call/3,handle_cast/2,
         terminate/2,code_change/3,handle_info/2]).
%% Non-gen_server interface functions
-export([init/0,newlog/1,alive/0,quit/0,spawn_link/0]).
%% Grab our db functions
-include("dbfuncs.erl").

%% Gets called implicitly by the gen_server:start_link call
init(Arg) ->
    print(init_table(database_log, [node()])),
    {ok, Arg}.

%% Synchronous method which traps signals from the gen_server and
%% handles their restarts the server upon a crash.
spawn_link() ->
    process_flag(trap_exit, true),
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    link(Pid),
    receive
        {'EXIT', Pid, terminate} ->
            print("Catching termination signal: Quitting.....");
        {'EXIT', Pid, Reason}->
            io:format("Caught signal: ~p. Restarting...~n", [Reason]),
            spawn_link()
    end.

%% Starts our services asynchronously
init() ->
    spawn(?MODULE, spawn_link, []).

%% Sends new data to the logger.
newlog(Data) ->
    gen_server:call(?MODULE, {newlog, Data}).
%% Pings the server to see if it is alive. If not, crashes.
alive() ->
    gen_server:cast(?MODULE, alive).
%% Terminates the server.
quit() ->
    gen_server:call(?MODULE, terminate).
%% Fired when the server dies.
terminate(Reason, State) ->
    io:format("Server was killed with reason: ~p~n", [Reason]),
    {Reason, State}.

%% Server request handlers
handle_call({newlog, Data}, _From, State) ->
    io:format("~p~n", [newlog]),
    {reply, add_entry(Data), State};
handle_call(alive, _From, State) ->
    {reply, true, State};
handle_call(terminate, _From, State) ->
    {stop, terminate, ok, State}.

handle_cast({newlog, Data}, State) ->
    {reply, add_entry(Data), State};
handle_cast(terminate, State) ->
    {stop, terminate, ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_info(_Info, State) ->
    {noreply, State}.
