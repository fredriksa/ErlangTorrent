%%%-------------------------------------------------------------------
%%% Created : 22. Mar 2019 19:22
%%%-------------------------------------------------------------------
-module(tracker).

%% API
-export([start/0, init/1, handle_call/3]).

%---------------------------------------------------------------------------------
%% Function: start/0.
%% Purpose: Starts the tracker gen server.
%% Args: No arguments.
%% Returns: The result of the start operation.
%%---------------------------------------------------------------------------------
start() ->
  EmptyMap = maps:new(),
  SeederMap = maps:put(seeders, maps:new(), EmptyMap),
  InterestNSeederMap = maps:put(interests, maps:new(), SeederMap),
  gen_server:start({local, tracker}, tracker, InterestNSeederMap, []).

%---------------------------------------------------------------------------------
%% Function: init/1.
%% Purpose: Initializes the tracker gen server.
%% Args: The first argument is the initial state for the index gen server.
%% Returns: A tuple {ok, N} where N is the initial state of the index gen server.
%%---------------------------------------------------------------------------------
init(N) ->
  {ok, N}.

%---------------------------------------------------------------------------------
%% Function: handle_call/3.
%% Purpose: Responsible for providing functionality for the i_have operation
%%          for the tracker gen server. The function lists the sending Pid as a
%%          seeder for the Md5 argument.
%% Args: The first argument is the request, the second argument is the sender and
%%       the final argument is the state of the index gen server.
%% Returns: A tuple consisting of the result of the request handling, including
%%          response to the caller. In this case the atom ok with the modified state.
%%---------------------------------------------------------------------------------
handle_call({i_have, Md5}, From, State) ->
  {FromPid, _} = From,
  SeederState = maps:get(seeders, State),
  ContainsMd5 = maps:is_key(Md5, SeederState),
  if
    ContainsMd5 ->
      Seeders = maps:get(Md5, SeederState),
      case lists:member(FromPid, Seeders) of
        true ->
          NewState = State;
        false ->
          NewSeeders = Seeders ++ [FromPid],
          NewState = maps:put(seeders, maps:put(Md5, NewSeeders, SeederState), State)
      end;
    true ->
      NewState = maps:put(seeders, maps:put(Md5, [FromPid], SeederState), State)
  end,
  {reply, ok, NewState};

%---------------------------------------------------------------------------------
%% Function: handle_call/3.
%% Purpose: Responsible for providing functionality for the i_am_interested_in
%%          operation for the tracker gen server. The function lists the sending
%%          pid as a pid interested in the passed in Md5.
%% Args: The first argument is the request, the second argument is the sender and
%%       the final argument is the state of the index gen server.
%% Returns: A tuple consisting of the result of the request handling, including
%%          response to the caller. In this case the atom ok with the modified state.
%%---------------------------------------------------------------------------------
handle_call({i_am_interested_in, Md5}, From, State) ->
  {FromPid, _} = From,
  InterestState = maps:get(interests, State),
  ContainsMd5 = maps:is_key(Md5, InterestState),
  if
    ContainsMd5 ->
      Senders = maps:get(Md5, InterestState),
      case lists:member(FromPid, Senders) of
        true ->
          NewState = State;
        false ->
          NewSenders = Senders ++ [FromPid],
          NewState = maps:put(interests, maps:put(Md5, NewSenders, InterestState), State)
      end;
    true ->
      NewState = maps:put(interests, maps:put(Md5, [FromPid], InterestState), State)
  end,
  {reply, ok, NewState};

%---------------------------------------------------------------------------------
%% Function: handle_call/3.
%% Purpose: Responsible for providing functionality for the who_is_interested_in
%%          operation for the tracker gen server. The function responds with all
%%          the PIDs of the peers interested in the file identified by Md5.
%% Args: The first argument is the request, the second argument is the sender and
%%       the final argument is the state of the index gen server.
%% Returns: A tuple consisting of the result of the request handling, including
%%          response to the caller. In this case the a list of all the interested
%%          parties and the unmodified state.
%%---------------------------------------------------------------------------------
handle_call({who_is_interested_in, Md5}, _, State) ->
  InterestState = maps:get(interests, State),
  case maps:is_key(Md5, InterestState) of
    true ->
      {reply, maps:get(Md5, InterestState), State};
    false ->
      {reply, [], State}
  end;
%---------------------------------------------------------------------------------
%% Function: handle_call/3.
%% Purpose: Responsible for providing functionality for the who_is_seeding
%%          operation for the tracker gen server. The function responds with all
%%          the PIDs of the peers seeding the file identified by Md5.
%% Args: The first argument is the request, the second argument is the sender and
%%       the final argument is the state of the index gen server.
%% Returns: A tuple consisting of the result of the request handling, including
%%          response to the caller. In this case a list of all the seeders
%%          and the unmodified state.
%%---------------------------------------------------------------------------------
handle_call({who_is_seeding,  Md5}, _, State) ->
  SeederState = maps:get(seeders, State),
  case maps:is_key(Md5, SeederState) of
    true ->
      {reply, maps:get(Md5, SeederState), State};
    false ->
      {reply, [], State}
  end.


