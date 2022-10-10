-module(opty).
-export([start/5, stop/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(Clients, Entries, Reads, Writes, Time) ->
    Snode = 'server@adrian',
    io:format("Creo server~n"),
    spawn(Snode, fun() -> 
                         register(s, server:start(Entries))end),
    io:format("Creado~n"),
    L = startClients(Clients, [], Entries, Reads, Writes),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n", 
         [Clients, Entries, Reads, Writes, Time]),
    timer:sleep(Time*1000),
    stop(L).

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    waitClients(L, []),
    io:format("Stopping server~n"),
    {s, 'server@adrian'} ! stop,
    io:format("Stopped~n").

startClients(0, L, _, _, _) -> L;
startClients(Clients, L, Entries, Reads, Writes) ->
    Snode = 'server@adrian',
    Pid = client:start(Clients, Entries, Reads, Writes, {s, Snode}),%Snode = s
    startClients(Clients-1, [Pid|L], Entries, Reads, Writes).

stopClients([]) ->
    ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},	
    stopClients(L).

waitClients([], SuccessRates) ->
    N = length(SuccessRates),
    Sum = lists:foldl(fun(X, Sum) -> X + Sum end, 0, SuccessRates),
    Mean = Sum / N,
    io:format("Mean success rate: ~w~n", [Mean]),
    Stddev = stddev(SuccessRates, Mean),
    io:format("Stddev of success rate: ~w~n", [Stddev]);
waitClients(L, SuccessRate) ->
    receive
        {done, Pid, ProcSuccessRate} ->
            waitClients(lists:delete(Pid, L), [ProcSuccessRate | SuccessRate])
    end.


stddev(ListofNumbers, Mean) ->
    N = length(ListofNumbers),
    {Tmp, Basura} = lists:foldl(fun(X, ErlangDaAsco) -> {Sum_, Mean_} = ErlangDaAsco, {Sum_ + math:pow(X-Mean_, 2), Mean_} end, {0, Mean}, ListofNumbers),
    Tmp2 = Tmp / N,
    math:sqrt(Tmp2).
