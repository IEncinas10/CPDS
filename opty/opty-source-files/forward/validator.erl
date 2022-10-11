-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init()->
    validator().

validator() ->
    receive
        {validate, Ref, Reads, Writes, Client} ->
            Tag = make_ref(),
	    % Send check request to every entry we've read
            send_write_check(Writes),  %% modify to check_writes
	    % Process the result
            case check_writes(length(Writes)) of  
                ok ->
		    % Transaction OK, flush writes and finish
                    update(Writes),
                    Client ! {Ref, ok};
                abort ->
		    % Transaction read stale data, abort
                  lists:foreach(fun(Entry) -> 
                  Entry ! {delete, self()}
                          end,
                          Writes),

		              Client ! {Ref, abort}
            end,
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.
    
update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) -> 
		  % Update each entry in our Writes
		  Entry ! {write, Value}
                  end, 
                  Writes).


%%TODO create send_write_check


send_write_check(Writes) ->
    Self = self(),
    lists:foreach(fun(Entry) -> 
    Entry ! {check, Self}
          end,
          Writes).

check_writes(0) ->
  ok;

check_writes(N) ->
  receive
    ok ->
      check_writes(N-1);
    abort ->
      abort
  end.


send_read_checks(Reads, Tag) ->
    Self = self(),
    lists:foreach(fun({Entry, Time}) -> 
		  % Check every Read we've made
		  Entry ! {check, Tag, Time, Self}
                  end, 
                  Reads).

check_reads(0, _) ->
    ok;
check_reads(N, Tag) ->
    receive
        {Tag, ok} ->
            check_reads(N-1, Tag);
        {Tag, abort} ->
            abort
    end.
