-module(opty).
-export([start/5, stop/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(Clients, Entries, Reads, Writes, Time) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Reads, Writes),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w RDxTR, ~w WRxTR, DURATION ~w s~n", 
         [Clients, Entries, Reads, Writes, Time]),
    timer:sleep(Time*1000),
    stop(L).

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    io:format("waiting for data...~n"),
    waitClients(L, []),
    io:format("stopping server..."),
    s ! stop,
    io:format("Stopped~n").

startClients(0, L, _, _, _) -> L;
startClients(Clients, L, Entries, Reads, Writes) ->
    Pid = client:start(Clients, Entries, Reads, Writes, s),
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
    {Tmp, _} = lists:foldl(fun(X, Aux) -> {Sum_, Mean_} = Aux, {Sum_ + math:pow(X-Mean_, 2), Mean_} end, {0, Mean}, ListofNumbers),
    Tmp2 = Tmp / N,
    math:sqrt(Tmp2).
