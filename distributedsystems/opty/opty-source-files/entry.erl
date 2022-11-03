-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, make_ref()).

entry(Value, Time) ->
    receive
        {read, Ref, From} ->
	    % We respond to the handler. 
	    % Value + {self(), Time} for checking
	    From ! {Ref, self(), Value, Time},
            entry(Value, Time);
        {write, New} ->
	    % We just update the value and the Time/Ref
            entry(New , make_ref());
        {check, Ref, Readtime, From} ->
            if 
                 Readtime == Time -> 
		    From ! {Ref, ok};
                true ->
                    From ! {Ref, abort}
            end,
            entry(Value, Time);
        stop ->
            ok
    end.
