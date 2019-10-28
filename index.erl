%%%-------------------------------------------------------------------
%%% Created : 22. Mar 2019 19:22
%%%-------------------------------------------------------------------
-module(index).

%% API
-export([start/0, init/1, handle_call/3]).

%---------------------------------------------------------------------------------
%% Function: start/0.
%% Purpose: Starts the index gen server.
%% Args: No arguments.
%% Returns: The result of the start operation.
%%---------------------------------------------------------------------------------
start() ->
  gen_server:start({local, index}, index, [], []).

%---------------------------------------------------------------------------------
%% Function: init/1.
%% Purpose: Initializes the index gen server.
%% Args: The first argument is the initial state for the index gen server.
%% Returns: A tuple {ok, N} where N is the initial state of the index gen server.
%%---------------------------------------------------------------------------------
init(N) ->
  {ok, N}.

%---------------------------------------------------------------------------------
%% Function: handle_call/3.
%% Purpose: Responsible for providing functionality for the add_to_index operation
%%          for the index gen server.
%% Args: The first argument is the request, the second argument is the sender and
%%       the final argument is the state of the index gen server.
%% Returns: A tuple consisting of the result of the request handling, including
%%          response to the caller. In this case the atom ok if the file was added
%%          to the index or the atom already_listed if not.
%%---------------------------------------------------------------------------------
handle_call({add_to_index, FileName, Md5, Size}, _, State) ->
  IndexData = {FileName, Md5, Size},
  IsMember = lists:member(IndexData, State),
  if
    IsMember == false ->
      {reply, ok, State ++ [IndexData]};
    true ->
      {reply, already_listed, State}
  end;

%---------------------------------------------------------------------------------
%% Function: handle_call/3.
%% Purpose: Responsible for providing functionality for the list operation for
%%          the index gen server.
%% Args: The first argument is the request, the second argument is the sender and
%%       the final argument is the state of the index gen server.
%% Returns: A tuple consisting of the result of the request handling, including
%%          response to the caller. In this case, all the listed files on the index
%%          server.
%% Note: This function is not part of the bit-torrent description, but to help
%%       with debugging or in the future implementing a index service where all
%%       the files and their hashes was available, I decided to extend this functionality.
%%---------------------------------------------------------------------------------
handle_call({list}, _, State) ->
  {reply, State, State}.


