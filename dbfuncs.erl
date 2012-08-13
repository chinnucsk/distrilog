-author("Aaron France").
-record(database_log, {id, level, host, time, info}).

%% Metadatabase functions
-export([init_table/2,id/0,id/1]).

%% Direct database manipulation functions
-export([add_entry/1,find_by_id/1]).

%% init_table creates the Mnesia database.
%%
%% init_table also checks if the ID broker is available and
%% if not, starts it.
init_table(WhichTable, Nodes) ->
    {idproc, node()} ! {restart, self()},
    receive
        ok ->
            ok
    after
        100 ->
            register(idproc, spawn(?MODULE, id, [0]))
    end,

    case mnesia:create_schema(Nodes) of
        ok ->
            mnesia:start(),
            mnesia:create_table(WhichTable, [{disk_copies, [node()]},
                                             {record_name, database_log},
                                             {attributes, record_info(fields, database_log)}]),
            ok;
        {error, Code} ->
            {error, Code}
    end.

%% Method blocks waiting for requests to return a new id.
%%
%% This method will atomically increment the ID value.
id(N) ->
    receive
        {newid, Pid} ->
            Pid ! N,
            id(N+1);
        {count, Pid} ->
            Pid ! N,
            id(N);
        {restart, Pid} ->
            Pid ! ok,
            logserver:id(N)            
    end.

%% Requests a new id from the id broker.
id() ->
    {idproc, node()} ! {newid, self()},
    receive
        N ->
            Id = N
    after
        100 ->
            Id = 1
    end,
    Id.

%% Adds an entry into the mnesia database.
add_entry(Data) ->
    {Level, Host, Time, D} = Data,
    Record = #database_log{id=id(),level=Level,host=Host,time=Time,info=D},
    F = fun() ->
                mnesia:write(Record)
        end,
    mnesia:transaction(F).

%% Finds an entry from the mnesia database using the index id key.
find_by_id(_) ->
    F = fun() ->
                mnesia:match_object(#database_log{})
        end,
    mnesia:transaction(F).
