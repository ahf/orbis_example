%%%
%%% Copyright (c) 2015 Alexander Færøy
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% ----------------------------------------------------------------------------
%%% @author Alexander Færøy <ahf@0x90.dk>
%%% @doc Orbis Example Worker.
%%% @end
%%% ----------------------------------------------------------------------------
-module(orbis_example_worker).
-behaviour(orbis_worker).
-behaviour(gen_server).

%% API.
-export([ping/1,
         crash/1]).

%% Orbis Worker.
-export([start_link/1]).

%% Generic Server Callbacks.
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    name      :: orbis:pool(),
    partition :: orbis_chash_bucket:partition()
}).

-define(SERVER, ?MODULE).

-spec ping(Worker :: pid()) -> {pong, map()} | {error, term()}.
ping(Worker) ->
    gen_server:call(Worker, ping).

-spec crash(Worker :: pid()) -> ok.
crash(Worker) ->
    gen_server:cast(Worker, crash).

-spec start_link([term()]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Arguments) ->
    gen_server:start_link(?SERVER, Arguments, []).

%% @private
init([Name, Partition | _Arguments]) ->
    {ok, #state {
        name = Name,
        partition = Partition
    }}.

%% @private
handle_call(ping, _From, #state { partition = Partition } = State) ->
    {reply, {pong, #{ partition => Partition, worker => self() }}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(crash, State) ->
    erlang:error(crash),
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
