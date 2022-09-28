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
  {Decision, LastRound} = round(Name, ?backoff, Round, Proposal, Acceptors, PanelId, Main),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n", 
             [Name, Decision, LastRound, Elapsed]),
  {Name, Main} ! done,
  PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId, Main) ->
  io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n", 
             [Name, Round, Proposal]),
  % Update gui
  PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
  case ballot(Name, Round, Proposal, Acceptors, PanelId, Main) of
    % Consensus, return {Value, Round} 
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      % Try again after sleeping, increment round and sleeptime
      Next = order:inc(Round),
      round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId, Main)
  end.

ballot(Name, Round, Proposal, Acceptors, PanelId, Main) ->
  % Send prepare message with round information
  prepare(Round, Acceptors, Main),
  % Necessary votes
  Quorum = (length(Acceptors) div 2) + 1,
  MaxVoted = order:null(),
  % Quorum vamos haciendole -1 hasta llegar a 0
  case collect(Quorum, Round, MaxVoted, Proposal) of
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      % We got promised, lets ask for votes
      accept(Round, Value, Acceptors, Main),
      case vote(Quorum, Round) of
        ok ->
          {ok, Value};
        abort ->
          abort
      end;
    abort ->
      abort
  end.

% No more votes needed, lets ask for votes
collect(0, _, _, Proposal) ->
  {accepted, Proposal};

% Normal case. 
collect(N, Round, MaxVoted, Proposal) ->
  receive 
    % Promise received, no previous votes. Keep collecting 'support'
    {promise, Round, _, na} ->
      collect(N-1, Round, MaxVoted, Proposal);
    {promise, Round, Voted, Value} ->
      % We got the promise. Update the maximum Voted/Proposal
      case order:gr(Voted, MaxVoted) of
	% Learn value
        true ->
          collect(N-1, Round, Voted, Value);
	% Keep this proposal
        false ->
          collect(N-1, Round, MaxVoted, Proposal)
      end;
    % TODO: Old message, ignore and keep going?
    {promise, _, _,  _} ->
      collect(N, Round, MaxVoted, Proposal);
    % Rejected, just keep gathering support
    {sorry, {prepare, Round}} ->
      collect(N, Round, MaxVoted, Proposal);
    % TODO: Old message from message or whatever?
    {sorry, _} ->
      collect(N, Round, MaxVoted, Proposal)
  after ?timeout ->
    io:format("[Proposer ~w] Timed out~n", 
               [self()]),
    abort
  end.

% Consensus!
vote(0, _) ->
  ok;
vote(N, Round) ->
  receive
    {vote, Round} ->
      vote(N-1, Round); % voto ganado, uno menos
    {vote, _} -> % voto desactualizado?
      vote(N, Round);
    {sorry, {accept, Round}} ->
      vote(N, Round); % Rejected, keep going
    {sorry, _} ->
      vote(N, Round) % Rejected from other round or from the promise
  after ?timeout ->
    io:format("[Proposer ~w] Timed out~n", 
               [self()]),
    abort
  end.

prepare(Round, Acceptors, Anode) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {prepare, self(), Round}, Anode) 
  end,
  lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors, Anode) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {accept, self(), Round, Proposal}, Anode) 
  end,
  lists:foreach(Fun, Acceptors).

send(Name, Message, ANode) ->
       {Name, ANode} ! Message.
