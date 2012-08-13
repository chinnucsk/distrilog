-author("Aaron France").
-record(database_log, {id, level, host, time, info}).
-export([init_table/2,add_entry/1,id/0,id/1]).

init_table(WhichTable, Nodes) ->
    register(idproc, spawn(?MODULE, id, [0])),
    case mnesia:create_schema(Nodes) of
        ok ->
            mnesia:start(),
            mnesia:create_table(WhichTable, []);
        {error, Code} ->
            Code
    end.

id(N) ->
    io:format("here"),
    receive
        {newid, Pid} ->
            Pid ! N,
            id(N+1);
        quit ->
            ok
    end.

id() ->
    {idproc, node()} ! {newid, self()},
    receive
        N ->
            Id = N
    after
        0 ->
            Id = 1
    end,
    Id.

add_entry(Data) ->
    {Level, Host, Time, D} = Data,
    Record = #database_log{id=id(), level=Level,host=Host,time=Time,info=D},
    F = fun() ->
                mnesia:write(Record)
        end,
    mnesia:transaction(F).
