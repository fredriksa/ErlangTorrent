%%%-------------------------------------------------------------------
%%% Created : 22. Mar 2019 19:22
%%%-------------------------------------------------------------------
-module(test).
-export([test/0]).


%---------------------------------------------------------------------------------
%% Function: test/0.
%% Purpose: Tests that the bit-torrent like functionality is working as intended.
%% Args: Nothing.
%% Notes:
%     This test is designed to run from the folder above src, compilation can be
%     performed by writing cover:compile_directory("./src/").
%% Returns: atom 'hooray' if the tests are successful.
%%---------------------------------------------------------------------------------
test() ->
  %% ##################
  %% Initial Test Setup
  %% ##################
  FooPath = "./src/foo.txt",
  FooTxtMd5 = helpers:compute_md5(FooPath),
  index:start(),
  tracker:start(),
  timer:sleep(50),
  {Peer1, Peer2, Peer3, Peer4} = {peer:new(), peer:new(), peer:new(), peer:new()},
  %% #################
  %% Environment Tests
  %% #################
  ok = rpcDebug("Peer 1: ", Peer1, {share_file, FooPath}),
  ok = rpcDebug("Peer 1: ", Peer1, {share_file, FooPath}),
  ok = rpcDebug("Peer 2: ", Peer2, {share_file, FooPath}),
  ok = rpcDebug("Peer 4: ", Peer4, {share_file, FooPath}),
  ok = rpcDebug("Peer 2: ", Peer2, {i_am_interested_in, FooPath}),
  ok = rpcDebug("Peer 2: ", Peer2, {i_am_interested_in, FooPath }),
  ok = rpcDebug("Peer 3: ", Peer3, {i_am_interested_in, FooPath}),
  Interested = rpcDebug("Peer 1 - Who is interested in foo.txt: ", Peer1, {who_is_interested_in, FooPath}),
  2 = length(Interested),
  Seeding = rpcDebug("Peer 1 - Who is seeding foo.txt", Peer1, {who_is_seeding, FooPath}),
  3 = length(Seeding),
  Publication = gen_server:call(index, {list}),
  1 = length(Publication),
  %% ###################
  %% Functionality Tests
  %% ###################
  all = rpcDebug("Peer 1 - Asks first interest what_have_you: ", Peer1, {lists:nth(1, Interested), what_have_you, FooTxtMd5 }),
  nothing = rpcDebug("Peer 2 - Asks non-interest what_have_you: ", Peer2, {Peer3, what_have_you, FooTxtMd5}),
  {file, FooTxtMd5, _} = rpcDebug("Ask for send files from Peer 1 (could be first interest)", Peer1, {send_me, FooTxtMd5 }),
  error = rpcDebug("Ask for send files from Peer 3", Peer3, {send_me, FooTxtMd5}),
  rpcDebug("Peer 3: ", Peer3, {share_file, "./src/foo.txt"}),
  {file, FooTxtMd5, _} = rpcDebug("Ask for send files from Peer 3", Peer3, {send_me, FooTxtMd5}),
  io:fwrite("Before find first seeder ~n"),
  FirstSeeder = rpcDebug("Ask for first seeder", Peer1, {find_first_seeder, FooPath}),
  all = rpcDebug("FirstSeeder: What have you", Peer1, {FirstSeeder, what_have_you, FooTxtMd5}),
  {file, FooTxtMd5, ReceivedData} = rpcDebug("AskFirstSeeder to send file", FirstSeeder, {send_me, FooTxtMd5}),
  {ok, OriginalData} = file:read_file(FooPath),
  true = (ReceivedData == OriginalData),
  io:fwrite("################################~n"),
  io:fwrite("## TEST SUIT RAN SUCCESSFULLY ##~n"),
  io:fwrite("################################~n"),
  hooray.

%---------------------------------------------------------------------------------
%% Function: rpcDebug/3.
%% Purpose: Performs a RPC call and prints a debug message together with
%%          the results of the call.
%% Args: Nothing.
%% Returns: The result of the RPC operation.
%%---------------------------------------------------------------------------------
rpcDebug(Debug, Pid, Message) ->
  R = helpers:rpc(Pid, Message),
  io:fwrite("#Debug: ~p => ~p ~n", [Debug, R]),
  R.


