-module(handler).
-export([start/3]).

-define(KeyPos, 1).

start(Client, Validator, Store) ->
    spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
    handler(Client, Validator, Store, [], []).

handler(Client, Validator, Store, Reads, Writes) ->         
    receive
        {read, Ref, N} ->
	    % See if we have written to this entry
            case lists:keyfind(N, 1, Writes) of  
                {N, _, Value} ->
		    % Send THIS value to the Client
		    Client ! {value, Ref, Value},
                    handler(Client, Validator, Store, Reads, Writes);
                false ->
		    % Find the corresponding Entry
		    Entry = store:lookup(N, Store),
		    % Request value to entry
		    Entry ! {read, Ref, self()},
                    handler(Client, Validator, Store, Reads, Writes)
            end;
        {Ref, Entry, Value} ->
	    % We got the reading value, send it to Client
	    Client ! {value, Ref, Value},
	    % Also, add this Read to the Client's Reads
            handler(Client, Validator, Store, [Entry | Reads], Writes);
        {write, N, Value} ->
	    % Find Entry
	    Entry = store:lookup(N, Store),
	    % Append (or update if existed) the entry in our Writes list
            NewWrites = lists:keystore(N, 1, Writes, {N, Entry, Value}),
            handler(Client, Validator, Store, Reads, NewWrites);
        {commit, Ref} ->
	    Validator ! {validate, Ref, Reads, Writes, Client};
        abort ->
            ok
    end.
