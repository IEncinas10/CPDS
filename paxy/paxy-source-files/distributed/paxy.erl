-module(paxy).
-export([stop/0, stop/1, crash/1, startDistributedProposer/2, startDistributedAcceptor/1, startDistributedProposersRegister/2]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(AMBER, {255,191,0}).
-define(AMETHYST, {153,102,204}).
-define(AO, {0,127,0}).

% https://www.erlang.org/doc/man/lists.html#split-2
-define(proposers, 3).
getproposers() ->
    Val = os:getenv("proposers"),
    case Val of
	false -> ?proposers;
	_ -> N = list_to_integer(Val), N
    end.

-define(acceptors, 5).
getacceptors() ->
    Val = os:getenv("acceptors"),
    case Val of
	false -> ?acceptors;
	_ -> N = list_to_integer(Val), N
    end.

% Sleep is a list with the initial sleep time for each proposer

%startDistributedAcceptor(PNode) ->

%  NumAcceptors = getacceptors(),
%  AcceptorNames = lists:sublist(
%		  ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", 
%		   "Acceptor e", "Acceptor f", "Acceptor g", "Acceptor h", 
%		   "Acceptor i", "Acceptor j", "Acceptor k", "Acceptor l", 
%                  "Acceptor m", "Acceptor n", "Acceptor o", "Acceptor p", 
%	   "Acceptor q", "Acceptor r", "Acceptor s"], NumAcceptors),
%  AccRegister = lists:sublist([a, b, c, d, e, f, g, h, i, j, k, 
%		   l, m, n, o, p, q, r, s], NumAcceptors),
%
%  {proposers, PNode} ! {accReg, AccRegister},
%
%  register(guiAcc, spawn(fun() -> gui:start_acceptors(AcceptorNames) end)),
%  guiAcc ! {reqStateAcc, self()},
%  receive
%    {reqStateAcc, State} ->
%      {AccIds} = State,
%        start_acceptors(AccIds, AccRegister)
%  end.
%

startDistributedAcceptor(PNode) ->
  AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", "Acceptor e"],
  AccRegister = [a, b, c, d, e],
  io:format("mando mensaje"),
  {proposers, PNode} ! {accReg, AccRegister},
  io:format("mensaje mandado"),
  % computing panel heights
  %AccPanelHeight = length(AcceptorNames)*50 + 0, %plus the spacer value
  register(gui_acceptors, spawn(fun() -> gui:start_acceptors(AcceptorNames) end)),
  gui_acceptors ! {reqStateAcc, self()},

  receive
    {reqStateAcc, State} ->
      {AccIds} = State,
      start_acceptors(AccIds, AccRegister)
  end.

startDistributedProposersRegister(Sleep, ANode) ->
  register(proposers, spawn(fun() -> startDistributedProposer(Sleep, ANode) end)).

startDistributedProposer(Sleep, ANode) ->

  NumProposers = getproposers(),
  ProposerNames = lists:sublist([
		     {"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, 
		     {"Proposer willard", ?BLUE}, {"Proposer pedro", ?AMBER}, 
		     {"Proposer juan", ?AMETHYST}, {"Proposer alfonsito", ?AO}, 
		     {"Proposer ignacio", ?BLUE}, {"Proposer adrian", ?AMBER}], 
		    NumProposers),
  
  PropInfo = lists:sublist([
		{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}, {pedro, ?AMBER}, 
		{juan, ?AMETHYST}, {alfonsito, ?AO}, {ignacio, ?BLUE}, {adrian, ?AMBER}], 
	       NumProposers),

  register(guiProp, spawn(fun() -> gui:start_proposers(ProposerNames) end)),
  guiProp ! {reqStateProp, self()},
  receive
    {reqStateProp, State} ->
      receive
        {accReg, AccRegister} ->
          {PropIds} = State,
          start_proposers(PropIds, PropInfo, AccRegister, Sleep, ANode)
      
    end
  end.

    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      io:format("si~n"),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

stop() ->
  stop(a),
  stop(b),
  stop(c),
  stop(d),
  stop(e),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      io:format("error ~w~n", [Name]),
      ok;
    Pid ->
      pers:delete(Name),
      Pid ! stop
  end.


crash(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      
      io:format("[Acceptor ~w] CRASHED~n", [Name]),
      pers:open(Name),
      {_, _, _, Pn} = pers:read(Name),
      Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
      pers:close(Name),
      unregister(Name),
      exit(Pid, "crash"),
      timer:sleep(2000),
      register(Name, acceptor:start(Name, na))
  end.
