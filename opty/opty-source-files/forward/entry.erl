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
        {write, Ref, New} ->
	          % We just update the value and the Time/Ref
            entry(New, ListReads);
        {check, Ref, From} -> %Modify how that is comprobed, look on the list of read if there is other PID (From)

            FilteredList = lists:filter(fun(X)-> X /= From end, ListReads),
            if 
                 FilteredList == [] -> 
                
		                From ! {Ref, ok};
                false ->
                    From ! {Ref, abort}
            end,
            entry(Value, ListReads);
        {delete, From} ->
            FilteredList = lists:filter(fun(X)-> X /= From end, ListReads),
            entry(Value, FilteredList);
        stop ->
            ok
    end.
