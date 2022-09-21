-module(acceptor).
-export([start/2]).

-define(delay, 200).

%T = rand:uniform(getdelay()),
%timer:send_after(T, Pid, Message),


% doesnt work, dont know why..
getdelay() -> 
    Val = os:getenv("delay"),
    case Val of
	false -> ?delay;
	_ -> N = list_to_integer(Val), io:format("N = ~w~n", [N]), N 
    end.

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  Promised = order:null(), 
  Voted = order:null(),
  rand:seed(exs1024s, {123, 123534, 345345}),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
	  % Send promise with our {Round(new), Voted(old), Value(old)} state 
        true ->
          %Proposer ! {promise, Round, Voted, Value},               
	  %Experiment 2.i) 
          Message = {promise, Round, Voted, Value},
	  T = rand:uniform(getdelay()),
	  timer:send_after(T, Proposer, Message),
	  %Experiment 2.i)

	  % We promised Round, we still have voted Voted
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                 [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},

	  % Update Promised to Round (Round > Promised)
          acceptor(Name, Round, Voted, Value, PanelId);

	  % If Promised > Round we send sorry message specifying which round we're refusing 
        false ->
          %Proposer ! {sorry, {prepare, Round}},
	  %Experiment 2.i) 
          Message = {sorry, {prepare, Round}},
	  T = rand:uniform(getdelay()),
	  timer:send_after(T, Proposer, Message),
	  %Experiment 2.i)
	  
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
	  % If Round >= Promised we vote for the proposal
      case order:goe(Round, Promised) of
        true ->
          %Proposer ! {vote, Round},
	  %Experiment 2.i) 
          Message = {vote, Round},
	  T = rand:uniform(getdelay()),
	  timer:send_after(T, Proposer, Message),
	  %Experiment 2.i)

	  % If Round >= Voted we update the highest numbered proposal
          case order:goe(Round, Voted) of
            true ->
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
	      % Update Voted and Value = {Round, Proposal}
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Round, Voted, PanelId)
          end;                            
        false ->
	  %TODO check Round/Voted
	  %Both work but Round is the correct one! 
	  %works because of the {sorry, _}
          %Proposer ! {sorry, {accept, Round}},
	  
	  %Experiment 2.i) 
          Message = {sorry, {accept, Round}},
	  T = rand:uniform(getdelay()),
	  timer:send_after(T, Proposer, Message),
	  %Experiment 2.i)
	  
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
