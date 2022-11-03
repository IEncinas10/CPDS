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
            io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n",
            [ClientID, Total, Ok, 100*Ok/Total]),
	    SuccessRate = 100 * Ok / Total,
            From ! {done, self(), SuccessRate},
            ok;
        {transaction, Time, Store} ->
            Tref = make_ref(),
            Handler = handler:start(self(), Tref, Time, Store),
            case do_transaction(ClientID, Entries, Reads, Writes, Handler, Tref) of
                ok ->
                    open(ClientID, Entries, Reads, Writes, Server, Total+1, Ok+1);
                abort ->
                    open(ClientID, Entries, Reads, Writes, Server, Total+1, Ok)
            end
    end.

do_transaction(_, _, 0, 0, Handler, Tref) ->
    do_commit(Handler, Tref);
do_transaction(ClientID, Entries, 0, Writes, Handler, Tref) ->
    case do_write(Entries, Handler, ClientID, Tref) of
        abort ->
            abort;
        ok ->
            do_transaction(ClientID, Entries, 0, Writes-1, Handler, Tref)
    end;
do_transaction(ClientID, Entries, Reads, 0, Handler, Tref) ->
    case do_read(Entries, Handler, Tref) of
        abort ->
            abort;
        _ ->
            do_transaction(ClientID, Entries, Reads-1, 0, Handler, Tref)
    end;
do_transaction(ClientID, Entries, Reads, Writes, Handler, Tref) ->
    Op = rand:uniform(),
    if Op >= 0.5 ->
         case do_read(Entries, Handler, Tref) of
             abort ->
                 abort;
             _ ->
                 do_transaction(ClientID, Entries, Reads-1, Writes, Handler, Tref)
         end;
       true -> 
         case do_write(Entries, Handler, ClientID, Tref) of
             abort ->
                 abort;
             ok ->
                 do_transaction(ClientID, Entries, Reads, Writes-1, Handler, Tref)
         end
    end.

do_read(Entries, Handler, Tref) ->
    Ref = make_ref(),
    Index = rand:uniform(length(Entries)),
    Num = lists:nth(Index, Entries),
    %Num = rand:uniform(Entries),
    Handler ! {read, Ref, Num},
    receive
        {value, Ref, {ok, Value}} -> 
            Value;
        {abort, Tref} ->
            abort
    end.

do_write(Entries, Handler, Value, Tref) ->
    Ref = make_ref(), 
    Index = rand:uniform(length(Entries)),
    Num = lists:nth(Index, Entries),
    %Num = rand:uniform(Entries),
    Handler ! {write, Ref, Num, Value},
    receive
        {value, Ref, ok} ->
            ok;
        {abort, Tref} ->
            abort
    end.

do_commit(Handler, Tref) ->
    Handler ! commit,
    receive
        {commit, Tref} ->
            ok;
        {abort, Tref} ->
            abort
    end.

