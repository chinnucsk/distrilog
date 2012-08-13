%%Simple data logger
-module(logserver).
-author("Aaron France").
-behaviour(gen_server).

%% gen_server interface functions
-export([init/1,handle_call/3,terminate/2]).
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

newlog(Data) ->
    gen_server:call(?MODULE, {newlog, Data}).

alive() ->
    gen_server:call(?MODULE, {alive}).

quit() ->
    gen_server:call(?MODULE, terminate);

terminate(Reason, State) ->
    io:format("Server was killed with reason: ~p~n", [Reason]),
    State.

handle_call({newlog, _Data}, _From, State) ->
    io:format("~p~n", [newlog]),
    {reply, true, State};
handle_call({alive}, _From, State) ->
    {reply, true, State};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.
