-module(client).
-export([start/5]).

%% StoreSubset: Size of the subset of stores that will be accessed (%)

-define(default_store_subset_percentage, 1).
getsubsetpercentage() ->
    Val = os:getenv("subset_percentage"),
    case Val of
	false -> ?default_store_subset_percentage;
	_ -> N = list_to_float(Val), N
    end.

sample(Entries, S) ->
    lists:sublist([X || {_ ,X} <- lists:sort([{rand:uniform(), E} || E <- lists:seq(1, Entries)])], S).

start(ClientID, Entries, Reads, Writes, Server) ->
    spawn(fun() -> 
	      P = getsubsetpercentage(),
	      SubEntries = trunc(P * Entries),
	      io:format("Percentage: ~w. Entries: ~w, SubEntries: ~w~n", [P, Entries, P*Entries]),
	      SubSetList = sample(Entries, SubEntries), 
	      io:format("[~w] SubSetList: ~w~n", [ClientID, SubSetList]),
	      open(ClientID, SubSetList, Reads, Writes, Server, 0, 0) end).

open(ClientID, Entries, Reads, Writes, Server, Total, Ok) ->
    Server ! {open, self()},
    receive
        {stop, From} ->
	          SuccessRate = 100 * Ok / Total,
            io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n",
            [ClientID, Total, Ok, SuccessRate]),
            From ! {done, self(), SuccessRate},
            ok;
        {transaction, Validator, Store} ->
            Handler = handler:start(self(), Validator, Store),
            case do_transaction(ClientID, Entries, Reads, Writes, Handler) of
                ok ->
                    open(ClientID, Entries, Reads, Writes, Server, Total+1, Ok+1);
                abort ->
                    open(ClientID, Entries, Reads, Writes, Server, Total+1, Ok)
            end
    end.

do_transaction(_, _, 0, 0, Handler) ->
    do_commit(Handler);
do_transaction(ClientID, Entries, 0, Writes, Handler) ->
    do_write(Entries, Handler, ClientID),
    do_transaction(ClientID, Entries, 0, Writes-1, Handler);
do_transaction(ClientID, Entries, Reads, 0, Handler) ->
    do_read(Entries, Handler),
    do_transaction(ClientID, Entries, Reads-1, 0, Handler);
do_transaction(ClientID, Entries, Reads, Writes, Handler) ->
    Op = rand:uniform(),
    if Op >= 0.5 ->
         do_read(Entries, Handler),
         do_transaction(ClientID, Entries, Reads-1, Writes, Handler);
       true -> 
         do_write(Entries, Handler, ClientID),
         do_transaction(ClientID, Entries, Reads, Writes-1, Handler)
    end.

do_read(Entries, Handler) ->
    Ref = make_ref(),
    Index = rand:uniform(length(Entries)),
    Num = lists:nth(Index, Entries),
    %Num = rand:uniform(Entries),
    Handler ! {read, Ref, Num},
    receive
        {value, Ref, Value} -> Value
    end.

do_write(Entries, Handler, Value) ->
    Index = rand:uniform(length(Entries)),
    Num = lists:nth(Index, Entries),
    %Num = rand:uniform(Entries),
    Handler ! {write, Num, Value}.

do_commit(Handler) ->
    Ref = make_ref(),
    Handler ! {commit, Ref},
    receive
        {Ref, Value} -> Value
    end.
