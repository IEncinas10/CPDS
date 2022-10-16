-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, []).

entry(Value, ListReads) -> %% TIME NOT NEEDED modify all related code with time
    receive
        {read, Ref, From} ->
            
            %%TODO  crear lista con las lecturas
            NewListReads = lists:append(ListReads, [From]),
	          From ! {Ref, self(), Value},
            entry(Value, NewListReads);
        {write, New} ->
            
	          % We just update the value and the Time/Ref
            entry(New, []);
        {check, From, Validator} -> %Modify how that is comprobed, look on the list of read if there is other PID (From)
            case length(ListReads) of
                0->
		                Validator !{ok};
                _ ->
                    Validator ! {abort}
            end,
            entry(Value, ListReads);
        {delete, From} ->
            NewListReads = remove(From, ListReads),
            entry(Value, NewListReads);
        stop ->
            ok
    end.



remove(_, []) -> [];
remove(H, [H|T]) ->
    remove(H, T);
remove(X, [H|T]) ->
    [H | remove(X, T)].
