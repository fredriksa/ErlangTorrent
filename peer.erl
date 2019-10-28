%%%-------------------------------------------------------------------
%%% Created : 22. Mar 2019 19:22
%%%-------------------------------------------------------------------
-module(peer).
-include_lib("kernel/include/file.hrl").

%% API
-export([new/0]).

%---------------------------------------------------------------------------------
%% Function:  new/0.
%% Purpose: Spawns a new peer process.
%% Args: Nothing.
%% Returns: The spawned peer PID.
%%---------------------------------------------------------------------------------
new() ->
  spawn(fun() -> peer(maps:new()) end).

%---------------------------------------------------------------------------------
%% Function: peer/1.
%% Purpose: The function representing a peer instance.
%% Args: The first argument is the peer's initial state.
%% Returns: Returns nothing, keeps calling itself forever.
%%---------------------------------------------------------------------------------
peer(State) ->
  receive
    Msg ->
      peer(handle_message(State, Msg))
  end.


%---------------------------------------------------------------------------------
%% Function: handle_message/2.
%% Purpose: Responsible for providing functionality for the what_have_you
%%          operation for the peer as the requester. Sends back the result to the
%%          sender.
%% Args: The first argument is the state of the peer, the second argument is the
%%        message to be handled. In this case, the sender PID, the recipient of
%%        the what_have_you operation and the atom what_have_you and finally the
%%        Md5 of the file to ask the peer what it has for.
%% Returns: The state of the peer.
%%---------------------------------------------------------------------------------
handle_message(State, {Sender, Recipient, what_have_you, Md5}) ->
  Recipient ! {self(), what_have_you, Md5},
  receive
    {i_have, Result} ->
      Sender ! Result
  end,
  State;


%---------------------------------------------------------------------------------
%% Function: handle_message/2.
%% Purpose: Responsible for providing functionality for the what_have_you
%%          operation for the peer as the respondent. Sends the tuple containing an
%%          atom all or nothing to the sender depending on if the recipient has the
%%          file available for seeding or not.
%% Args: The first argument is the state of the peer, the second argument is the
%%        message to be handled. In this case, the sender PID, the atom
%%        what_have_you and the FilePath of the Md5 for the peer to check if
%%        it has registered for sharing.
%% Returns: The state of the peer.
%%---------------------------------------------------------------------------------
handle_message(State, {Sender, what_have_you, Md5}) ->
  case maps:is_key(Md5, State) of
    true ->
      {FilePath, _} = maps:get(Md5, State),
      case helpers:verify(FilePath, Md5) of
        true ->
          Sender ! {i_have, all};
        false ->
          Sender ! {i_have, nothing}
      end;
     false ->
        Sender ! {i_have, nothing}
  end,
  State;


%---------------------------------------------------------------------------------
%% Function: handle_message/2.
%% Purpose: Responsible for providing functionality for the find_first_seeder
%%          operation for the peer. Finds a random first seeder from the file's
%%          seeder list and sends one random seeder Pid to the sender.
%% Args: The first argument is the state of the peer, the second argument is the
%%        message to be handled. In this case, the sender PID, the atom
%%        find_first_seeder and the FilePath of the file for the peer to
%%        find the first seeder for.
%% Returns: The state of the peer.
%%---------------------------------------------------------------------------------
handle_message(State, {Sender, find_first_seeder, FilePath}) ->
  Md5 = helpers:compute_md5(FilePath),
  Candidates = gen_server:call(tracker, {who_is_seeding, Md5}),
  Sender ! lists:nth(rand:uniform(length(Candidates)), Candidates),
  State;


%---------------------------------------------------------------------------------
%% Function: handle_message/2.
%% Purpose: Responsible for providing functionality for the send_me
%%          operation for the peer. Sends the read file data to the sender.
%% Args: The first argument is the state of the peer, the second argument is the
%%        message to be handled. In this case, the sender PID, the atom
%%        send_me and the Md5 that the sender wants to be sent.
%% Returns: The state of the peer.
%% Notes: Because the instructions says that the problem should be simplified and
%%        that the files are so small we can transfer the entire file in one message.
%%        Because we transfer the entire message at a time we do not need to have
%%        tuples describing which parts of the file that a peer has available.
%%---------------------------------------------------------------------------------
handle_message(State, {Sender, send_me, Md5}) ->
  case maps:is_key(Md5, State) of
    true ->
      {FilePath, _} = maps:get(Md5, State),
      case helpers:verify(FilePath, Md5) of
        true ->
           {ok, Data} = file:read_file(FilePath),
            Sender ! {file, Md5, Data};
        false ->
          Sender ! error
      end;
    false ->
      Sender ! error
  end,
  State;

%---------------------------------------------------------------------------------
%% Function: handle_message/2.
%% Purpose: Responsible for providing functionality for the share_file
%%          operation for the peer. Sends the atom ok to the sender once
%%          the call is completed.
%% Args: The first argument is the state of the peer, the second argument is the
%%        message to be handled. In this case, the sender PID, the atom
%%        share_file and the FilePath of the file for the peer to share.
%% Returns: The state of the peer.
%%---------------------------------------------------------------------------------
handle_message(State, {Sender, share_file, FilePath}) ->
  Md5 = helpers:compute_md5(FilePath),
  {ok, FileInfo} = file:read_file_info(FilePath),
  Size = FileInfo#file_info.size,
  FileName = filename:basename(FilePath),
  ok = gen_server:call(tracker, {i_have, Md5}),
  gen_server:call(index, {add_to_index, FileName, Md5, Size}),
  Sender ! ok,
  maps:put(Md5, {FilePath, FileName}, State);


%---------------------------------------------------------------------------------
%% Function: handle_message/2.
%% Purpose: Responsible for providing functionality for the i_am_interested_in
%%          operation for the peer. Sends the atom ok to the sender once call
%%          is completed.
%% Args: The first argument is the state of the peer, the second argument is the
%%        message to be handled. In this case, the sender PID, the atom
%%        i_am_interested_in and the FilePath of the file to say that the peer is
%%        interested in.
%% Returns: The state of the peer.
%%---------------------------------------------------------------------------------
handle_message(State, {Sender, i_am_interested_in, FilePath}) ->
  Md5 = helpers:compute_md5(FilePath),
  ok = gen_server:call(tracker, {i_am_interested_in, Md5}),
  Sender ! ok,
  State;

%---------------------------------------------------------------------------------
%% Function: handle_message/2.
%% Purpose: Responsible for providing functionality for the who_is_interested_in
%%          operation for the peer. Sends the response of the who_is_interested_in operation
%%          from the tracker gen_server to the sender.
%% Args: The first argument is the state of the peer, the second argument is the message
%%       to be handled. In this case, the sender PID, the atom who_is_interested_in
%%        and the FilePath of the file to check for who is seeding.
%% Returns: The state of the peer.
%%---------------------------------------------------------------------------------
handle_message(State, {Sender, who_is_interested_in, FilePath}) ->
  Md5 = helpers:compute_md5(FilePath),
  Response = gen_server:call(tracker, {who_is_interested_in, Md5}),
  Sender ! Response,
  State;

%---------------------------------------------------------------------------------
%% Function: handle_message/2.
%% Purpose: Responsible for providing functionality for the who_is_seeding operation
%%          for the peer. Sends the response of the who_is_seeding operation from
%%          the tracker gen_server to the sender.
%% Args: The first argument is the state of the peer, the second argument is the message
%%       to be handled. In this case, the sender PID, the atom who_is_seeding and the
%%       FilePath of the file to check for who is seeding.
%% Returns: The state of the peer.
%%---------------------------------------------------------------------------------
handle_message(State, {Sender, who_is_seeding, FilePath}) ->
  Md5 = helpers:compute_md5(FilePath),
  Response = gen_server:call(tracker, {who_is_seeding, Md5}),
  Sender ! Response,
  State.

