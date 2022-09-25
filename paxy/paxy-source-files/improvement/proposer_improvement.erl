-module(proposer).
-export([start/6]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Main) end).

init(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  timer:sleep(Sleep),
  Begin = erlang:monotonic_time(),
  Round = order:first(Name),
  {Decision, LastRound} = round(Name, ?backoff, Round, Proposal, Acceptors, PanelId),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n", 
             [Name, Decision, LastRound, Elapsed]),
  Main ! done,
  PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
  io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n", 
             [Name, Round, Proposal]),
  % Update gui
  PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
  case ballot(Name, Round, Proposal, Acceptors, PanelId) of
    % Consensus, return {Value, Round} 
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      % Try again after sleeping, increment round and sleeptime
      Next = order:inc(Round),
      round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
  end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
  % Send prepare message with round information
  prepare(Round, Acceptors),
  % Necessary votes
  Quorum = (length(Acceptors) div 2) + 1,
  MaxVoted = order:null(),
  % Quorum vamos haciendole -1 hasta llegar a 0
  case collect(Quorum, Round, MaxVoted, Proposal, Quorum) of
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      % We got promised, lets ask for votes
      accept(Round, Value, Acceptors),
      case vote(Quorum, Round, Quorum) of
        ok ->
          {ok, Value};
        abort ->
          abort
      end;
    abort ->
      abort
  end.

% No more votes needed, lets ask for votes
collect(0, _, _, Proposal, _) ->
  {accepted, Proposal};

collect(_, _, _, _, 0) ->
  abort;

% Normal case. 
collect(N, Round, MaxVoted, Proposal,Sorrys) ->
  receive 
    % Promise received, no previous votes. Keep collecting 'support'
    {promise, Round, _, na} ->
      collect(N-1, Round, MaxVoted, Proposal, Sorrys);
    {promise, Round, Voted, Value} ->
      % We got the promise. Update the maximum Voted/Proposal
      case order:gr(Voted, MaxVoted) of
	% Learn value
        true ->
          collect(N-1, Round, Voted, Value, Sorrys);
	% Keep this proposal
        false ->
          collect(N-1, Round, MaxVoted, Proposal, Sorrys)
      end;
    % TODO: Old message, ignore and keep going?
    {promise, _, _,  _} ->
      collect(N, Round, MaxVoted, Proposal, Sorrys);
    % Rejected, just keep gathering support
    {sorry, {prepare, Round}} ->
      collect(N, Round, MaxVoted, Proposal, Sorrys-1);
    % TODO: Old message from message or whatever?
    {sorry, _} ->
      collect(N, Round, MaxVoted, Proposal, Sorrys)
  after ?timeout ->
    io:format("[Proposer ~w] Timed out~n", 
               [self()]),
    abort
  end.

% Consensus!
vote(0, _, _) ->
  ok;
vote(_, _, 0) ->
  abort;
vote(N, Round, Sorrys) ->
  receive
    {vote, Round} ->
      vote(N-1, Round, Sorrys); % voto ganado, uno menos
    {vote, _} -> % voto desactualizado?
      vote(N, Round, Sorrys);
    {sorry, {accept, Round}} ->
      vote(N, Round, Sorrys-1); % Rejected, keep going
    {sorry, _} ->
      vote(N, Round, Sorrys) % Rejected from other round or from the promise
  after ?timeout ->
    io:format("[Proposer ~w] Timed out~n", 
               [self()]),
    abort
  end.

prepare(Round, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {prepare, self(), Round}) 
  end,
  lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {accept, self(), Round, Proposal}) 
  end,
  lists:foreach(Fun, Acceptors).

send(Name, Message) ->
  Name ! Message.
