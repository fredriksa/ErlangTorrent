%%%-------------------------------------------------------------------
%%% Created : 22. Mar 2019 19:22
%%%-------------------------------------------------------------------
-module(helpers).

%% API
-export([compute_md5/1, rpc/2, verify/2]).

%---------------------------------------------------------------------------------
%% Function: compute_md5/1.
%% Purpose: Computes the MD5 for a file located at a FilePath.
%% Args: The first argument is the FilePath of the file to compute the Md5 for.
%% Returns: The MD5 of the file.
%%---------------------------------------------------------------------------------
compute_md5(FilePath) ->
  {ok, Data} = file:read_file(FilePath),
  Initial = erlang:md5_init(),
  Updated = erlang:md5_update(Initial, Data),
  erlang:md5_final(Updated).

%---------------------------------------------------------------------------------
%% Function: rpc/2.
%% Purpose: Performs a RPC on a process identifier together with a specific message.
%% Args: The first argument is the process identifier to perform the RPC on,
%%       the second argument is the message to send to the Pid.
%% Returns: The response from the RPC operation.
%%---------------------------------------------------------------------------------
rpc(Pid, Message) ->
  Pid ! list_to_tuple([self()] ++ tuple_to_list(Message)),
  receive
    Any ->
      Any
  end.

%---------------------------------------------------------------------------------
%% Function: verify/2.
%% Purpose: Verifies that the file exists and that the Md5 matches a provided Md5.
%% Args: The first argument is the FilePath of the file to check if the Md5 matches for,
%%       the second argument is the MD5 to test if the file matches with.
%% Returns: The MD5 of the file.
%%---------------------------------------------------------------------------------
verify(FilePath, Md5) ->
  IsFile = filelib:is_file(FilePath),
  Md5Test = (Md5 == compute_md5(FilePath)),
  IsFile and Md5Test.
