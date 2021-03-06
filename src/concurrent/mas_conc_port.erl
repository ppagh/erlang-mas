%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc The module of a migration arena

-module(mas_conc_port).
-behaviour(gen_server).

-define(TIMEOUT,10000).

%% API
-export([start_link/2, giveArenas/2, immigrate/2, emigrate/2, close/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("mas.hrl").

-type agent() :: mas:agent().
-type arenas() :: mas_conc_supervisor:arenas().

%% ====================================================================
%% API functions
%% ====================================================================
-spec start_link(pid(), config()) -> {ok,pid()}.
start_link(Supervisor, Cf) ->
    gen_server:start_link(?MODULE, [Supervisor, Cf], []).

-spec giveArenas(pid(), arenas()) -> ok.
giveArenas(Pid, Arenas) ->
    gen_server:call(Pid, {arenas, Arenas}).

-spec immigrate(pid(), tuple()) -> ok.
immigrate(Pid, AgentInfo) ->
    gen_server:cast(Pid, {immigrant, AgentInfo}).

%% @doc Sends a request with an agent to the port.
-spec emigrate(pid(), agent()) -> arenas().
emigrate(Pid, Agent) ->
    gen_server:call(Pid, {emigrate, Agent}, infinity).

-spec close(pid()) -> ok.
close(Pid) ->
    gen_server:cast(Pid, close).

%% ====================================================================
%% Callbacks
%% ====================================================================
-record(state, {mySupervisor :: pid(),
                arenas :: arenas(),
                config :: config()}).

-type state() :: #state{} | cleaning.


-spec init([pid()]) -> {ok, state()} |
                       {ok, state(), non_neg_integer()}.
init([Supervisor, Cf]) ->
    mas_misc_util:seed_random(),
    {ok, #state{mySupervisor = Supervisor,
                config = Cf}}.


-spec handle_call(term(), {pid(), term()}, state()) ->
                         {reply, term(), state()} |
                         {reply, term(),
                          state(),hibernate | infinity | non_neg_integer()} |
                         {noreply, state()} |
                         {noreply, state(),
                          hibernate | infinity | non_neg_integer()} |
                         {stop, term(), term(), state()} |
                         {stop, term(), state()}.
handle_call({arenas, Arenas}, _From, St) ->
    mas_topology:helloPort(),
    {reply, ok, St#state{arenas = Arenas}};

handle_call({emigrate, _Agent}, {Pid, _}, cleaning) ->
    exit(Pid, finished),
    {noreply, cleaning, ?TIMEOUT};

handle_call({emigrate, Agent}, From, St = #state{mySupervisor = Sup}) ->
    mas_topology:emigrant({Agent, From}),
    exometer:update([Sup, migration], 1),
    {noreply, St}.


-spec handle_cast(term(), state()) ->
                         {noreply, state()} |
                         {noreply, state(),
                          hibernate | infinity | non_neg_integer()} |
                         {stop,term(),state()}.

handle_cast({immigrant, {_Agent, {Pid, _}}}, cleaning) ->
    exit(Pid, finished),
    {noreply, cleaning, ?TIMEOUT};

handle_cast({immigrant, {_Agent, From}}, St) ->
    gen_server:reply(From, St#state.arenas),
    {noreply, St};

handle_cast(close, _St) ->
    {noreply, cleaning, ?TIMEOUT}.


-spec handle_info(term(), state()) ->
                         {noreply, state()} |
                         {noreply, state(),
                          hibernate | infinity | non_neg_integer()} |
                         {stop, term(), state()}.
handle_info(timeout, cleaning) ->
    {stop, normal, cleaning}.


-spec terminate(term(), state()) -> no_return().
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
