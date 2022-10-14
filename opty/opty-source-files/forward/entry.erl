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
            entry(New, ListReads);
        {check, From} -> %Modify how that is comprobed, look on the list of read if there is other PID (From)
            io:format("checking list~n"),
            case length(ListReads) of
                0->
                    io:format("ok"), 
		                From !{ok};
                _ ->
                    io:format("~w~n", [length(ListReads)]),
                    From ! {abort}
            end,
            entry(Value, ListReads);
        {delete, From} ->
            remove(From, ListReads),
            entry(Value, ListReads);
        stop ->
            ok
    end.


%remove(X, L) ->
%    [Y || Y <- L, Y =/= X].

remove(Elem, L)->
  case lists:delete(Elem,L) of
    L    -> L;
    Rest -> remove(Elem,Rest)
  end.
