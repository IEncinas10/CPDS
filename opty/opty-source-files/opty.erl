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
    waitClients(L, []),
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
    SuccessRate = lists:foldl(fun(X, Prod) -> X * Prod end, 1, SuccessRates),
    io:format("Geomean success rate: ~w~n", [nth_root(length(SuccessRates), SuccessRate)]),
    Stddev = stddev(SuccessRates),
    io:format("Stddev of success rate: ~w~n", [Stddev]),
    SuccessRate;
waitClients(L, SuccessRate) ->
    receive
        {done, Pid, ProcSuccessRate} ->
            waitClients(lists:delete(Pid, L), [ProcSuccessRate | SuccessRate])
    end.


stddev(ListofNumbers) ->
    N = length(ListofNumbers),
    Sum = lists:foldl(fun(X, Sum) -> X + Sum end, 0, ListofNumbers),
    Mean = Sum / N,
    {Tmp, Basura} = lists:foldl(fun(X, ErlangDaAsco) -> {Sum_, Mean_} = ErlangDaAsco, {Sum_ + math:pow(X-Mean_, 2), Mean_} end, {0, Mean}, ListofNumbers),
    Tmp2 = Tmp / N,
    math:sqrt(Tmp2).

%https://rosettacode.org/wiki/Nth_root#Erlang
nth_root(N, X) -> nth_root(N, X, 1.0e-5).
nth_root(N, X, Precision) ->
    F = fun(Prev) -> ((N - 1) * Prev + X / math:pow(Prev, (N-1))) / N end,
    fixed_point(F, X, Precision).

fixed_point(F, Guess, Tolerance) ->
    fixed_point(F, Guess, Tolerance, F(Guess)).
fixed_point(_, Guess, Tolerance, Next) when abs(Guess - Next) < Tolerance ->
    Next;
fixed_point(F, _, Tolerance, Next) ->
    fixed_point(F, Next, Tolerance, F(Next)).

