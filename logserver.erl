%%Simple data logger
-module(logserver).
-author("Aaron France").
-behaviour(gen_server).

%% gen_server interface functions
-export([init/1,handle_call/3,handle_cast/2,
         terminate/2,code_change/3,handle_info/2]).
%% Non-gen_server interface functions
-export([init/0,newlog/1,alive/0,quit/0]).
%% Grab our db functions
-include("dbfuncs.erl").

init(Arg) ->
    init_table(database_log, [node()]),
    {ok, Arg}.

%% Starts our services
init() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

%% Server interaction functions
newlog(Data) ->
    gen_server:call(?MODULE, {newlog, Data}).
alive() ->
    gen_server:call(?MODULE, {alive}).
quit() ->
    gen_server:call(?MODULE, terminate).

%% Fired when the server dies.
terminate(Reason, State) ->
    io:format("Server was killed with reason: ~p~n", [Reason]),
    State.

%% Server request handlers
handle_call({newlog, Data}, _From, State) ->
    io:format("~p~n", [newlog]),
    add_entry(Data),
    {reply, true, State};
handle_call({alive}, _From, State) ->
    {reply, true, State};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({newlog, _Data}, State) ->
    io:format("~p~n", [newlog]),
    {reply, true, State};
handle_cast(terminate, State) ->
    {stop, normal, ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
