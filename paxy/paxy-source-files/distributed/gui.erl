-module(gui).
-export([start_acceptors/1, start_proposers/1, make_window_acceptors/2, make_window_proposers/2]).
-include_lib("wx/include/wx.hrl").

-define(WindowSize, {450, 420}).
-define(PanelSize, {175, 40}).
-define(OuterSizerMinWidth, 190).
-define(OuterSizerMaxHeight, 420).	% maximum sizer size
-define(InSizerMinWidth, 175).
-define(InSizerMinHeight, 40).
-define(PropTitle, "Proposers").
-define(PropText1, "Round:").
-define(AccTitle, "Acceptors").
-define(AccText1, "Voted: {}").
-define(AccText2, "Promised: {}").

start_acceptors(Acceptors) ->
  % computing panel heights (plus the spacer value)
  AccPanelHeight = length(Acceptors)*?InSizerMinHeight + 10, 
  State = make_window_acceptors(Acceptors, AccPanelHeight),
  gui(State).


start_proposers(Proposers) ->
  % computing panel heights (plus the spacer value)
  PropPanelHeight = length(Proposers)*?InSizerMinHeight + 10,
  State = make_window_proposers(Proposers, PropPanelHeight),
  gui(State).
 

make_window_acceptors(Acceptors, AccPanelHeight) ->
  Server = wx:new(),
  Env = wx:get_env(),
  Frame = wxFrame:new(Server, -1, "Paxos Algorithm", [{size,?WindowSize}]),
  wxFrame:connect(Frame, close_window),
  Panel  = wxPanel:new(Frame),

  % create Sizers
  OuterSizer = wxBoxSizer:new(?wxVERTICAL),
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  AcceptorSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
                                       [{label, "Acceptors"}]),

  % set Sizer's min width/height
  case AccPanelHeight > ?OuterSizerMaxHeight of
    true ->
      OuterAccSizerHeight = ?OuterSizerMaxHeight;
    false ->
      OuterAccSizerHeight = AccPanelHeight
  end,


  wxSizer:setMinSize(AcceptorSizer, ?OuterSizerMinWidth, OuterAccSizerHeight),
  % add spacers
  wxSizer:addSpacer(MainSizer, 10),  %spacer
  wxSizer:addSpacer(AcceptorSizer, 5),  

  % add ProposerSizer into MainSizer

  % add AcceptorSizer into MainSizer
  wxSizer:add(MainSizer, AcceptorSizer,[]),
  wxSizer:addSpacer(MainSizer, 10),
  wxSizer:addSpacer(OuterSizer, 10),

  % add MainSizer into OuterSizer
  wxSizer:add(OuterSizer, MainSizer, []),
 
  %% Now 'set' OuterSizer into the Panel
  wxPanel:setSizer(Panel, OuterSizer),

  % create Acceptors and Proposers Panels
  AccIds = create_acceptors(Acceptors, Panel, AcceptorSizer, Env),
  wxFrame:show(Frame),
  {Frame, AccIds}.



%%TODO hacer split de windows y cambiar llamada funciones en paxy
make_window_proposers(Proposers, PropPanelHeight) ->
  Server = wx:new(),
  Env = wx:get_env(),
  Frame = wxFrame:new(Server, -1, "Paxos Algorithm", [{size,?WindowSize}]),
  wxFrame:connect(Frame, close_window),
  Panel  = wxPanel:new(Frame),

  % create Sizers
  OuterSizer = wxBoxSizer:new(?wxVERTICAL),
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),

  ProposerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
                                       [{label, "Proposers"}]),
  % set Sizer's min width/height

  case PropPanelHeight > ?OuterSizerMaxHeight of
    true ->
      OuterPropSizerHeight = ?OuterSizerMaxHeight;
    false ->
      OuterPropSizerHeight = PropPanelHeight
  end,

  wxSizer:setMinSize(ProposerSizer, ?OuterSizerMinWidth, OuterPropSizerHeight),
  % add spacers
  wxSizer:addSpacer(MainSizer, 10),  %spacer
  wxSizer:addSpacer(ProposerSizer, 5),  

  % add ProposerSizer into MainSizer
  wxSizer:add(MainSizer, ProposerSizer,[]),
  wxSizer:addSpacer(MainSizer, 20),

  % add AcceptorSizer into MainSizer
  % add MainSizer into OuterSizer
  wxSizer:add(OuterSizer, MainSizer, []),
 
  %% Now 'set' OuterSizer into the Panel
  wxPanel:setSizer(Panel, OuterSizer),

  % create Acceptors and Proposers Panels
  PropIds = create_proposers(Proposers, Panel, ProposerSizer, Env),

  wxFrame:show(Frame),
  {Frame, PropIds}.


gui(State) ->
  {Frame, Ids} = State,
  receive
    % request State
    {reqStateProp, From} ->

      io:format("[Gui proposer] state requested ~n"),
      From ! {reqStateProp, {Ids}},
      gui(State);
    
    {reqStateAcc, From} ->

      io:format("[Gui accpetor] state requested ~n"),
      From ! {reqStateAcc, {Ids}},
      gui(State);
    % a connection gets the close_window signal
    % and sends this message to the server
    #wx{event=#wxClose{}} ->
      %optional, goes to shell
      io:format("[Gui] ~p closing window ~n", [self()]),
      % now we use the reference to Frame
      wxWindow:destroy(Frame),
      ok;  % we exit the loop
    stop ->
      wxWindow:destroy(Frame),
      ok;  % we exit the loop
    Msg ->
      %Everything else ends up here
      io:format("[Gui] unknown message: ~p ~n", [Msg]),
      gui(State)
  end.

% create acceptors
create_acceptors(AcceptorList, Panel, AcceptorSizer, Env) ->
  AcceptorData = lists:map(fun(AccTitle) ->
    AcceptorSizerIn = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
    [{label, AccTitle}]),
    %set Sizer's min width/height
    wxSizer:setMinSize(AcceptorSizerIn, ?InSizerMinWidth, ?InSizerMinHeight),
    AcceptorPanel = wxPanel:new(Panel, [{size, ?PanelSize}]),
    {Lb1, Lb2} = setPanel2(AcceptorPanel, ?wxBLACK, ?AccText1, ?AccText2),
    wxSizer:add(AcceptorSizerIn, AcceptorPanel),
    wxSizer:add(AcceptorSizer, AcceptorSizerIn),
    {AcceptorPanel, AcceptorSizerIn, Lb1, Lb2}
  end,
  AcceptorList),

  lists:map(fun({AcceptorPanel, AcceptorSizerIn, Lb1, Lb2}) ->
    spawn(fun() -> 
      wx:set_env(Env),
      acceptor(AcceptorPanel, AcceptorSizerIn, Lb1, Lb2)
    end)
  end,
  AcceptorData).

% create proposers
create_proposers(ProposerList, Panel, ProposerSizer, Env) ->
  ProposerData = lists:map(fun({PropTitle, TextColour}) ->
    ProposerSizerIn = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
    [{label, PropTitle}]),
    % set Sizer's min width/height
    wxSizer:setMinSize(ProposerSizerIn, ?InSizerMinWidth, ?InSizerMinHeight),
    ProposerPanel = wxPanel:new(Panel, [{size, ?PanelSize}]),
    Lb1 = setPanel(ProposerPanel, ?wxBLACK, ?PropText1),
    wxSizer:add(ProposerSizerIn, ProposerPanel),
    wxSizer:add(ProposerSizer, ProposerSizerIn),
    StaticBox = wxStaticBoxSizer:getStaticBox(ProposerSizerIn),
    wxStaticText:setForegroundColour(StaticBox, TextColour),
    {ProposerPanel, ProposerSizerIn, Lb1}
  end,
  ProposerList),

  lists:map(fun({ProposerPanel, ProposerSizerIn, Lb1}) -> 
    spawn(fun() -> 
      wx:set_env(Env),
      proposer(ProposerPanel, ProposerSizerIn, Lb1)
    end)
  end,
  ProposerData).

% acceptor loop waiting updates
acceptor(AccPanel, AccSizerIn, L1Obj, L2Obj) ->
  receive
    % update panel
    {updateAcc, NewL1, NewL2, Colour} ->
      updatePanel2(AccPanel, L1Obj, L2Obj, NewL1, NewL2, Colour),
      wxWindow:fit(AccPanel),
      acceptor(AccPanel, AccSizerIn, L1Obj, L2Obj);
    stop ->
      ok
  end.

% proposer loop waiting for updates
proposer(PropPanel, PropSizerIn, L1Obj) ->
  receive
    % update panel
    {updateProp, NewL1, Colour} ->
      updatePanel(PropPanel, L1Obj, NewL1, Colour),
      wxWindow:fit(PropPanel),
      proposer(PropPanel, PropSizerIn, L1Obj);
    stop ->
      ok
  end.

% set a Panel
setPanel2(InPanel, BgColour, L1Text, L2Text) ->
  wxPanel:setBackgroundColour(InPanel, BgColour),
  Label1Obj = wxStaticText:new(InPanel, 1, L1Text, [{pos, {5, 5}}]),
  wxStaticText:setForegroundColour(Label1Obj, ?wxWHITE),
  Label2Obj = wxStaticText:new(InPanel, 1, L2Text, [{pos, {5, 20}}]),
  wxStaticText:setForegroundColour(Label2Obj, ?wxWHITE),
  {Label1Obj, Label2Obj}.	

setPanel(InPanel, BgColour, L1Text) ->
  wxPanel:setBackgroundColour(InPanel, BgColour),
  Label1Obj = wxStaticText:new(InPanel, 1, L1Text, [{pos, {5, 12}}]),
  wxStaticText:setForegroundColour(Label1Obj, ?wxWHITE),
  Label1Obj.	

updatePanel2(Panel, Label1Obj, Label2Obj, NewL1, NewL2, Colour) ->
  wxPanel:setBackgroundColour(Panel, Colour),
  wxStaticText:setLabel(Label1Obj, NewL1),
  wxStaticText:setLabel(Label2Obj, NewL2),
  wxPanel:refresh(Panel).

updatePanel(Panel, Label1Obj, NewL1, Colour) ->
  wxPanel:setBackgroundColour(Panel, Colour),
  wxStaticText:setLabel(Label1Obj, NewL1),
  wxPanel:refresh(Panel).
