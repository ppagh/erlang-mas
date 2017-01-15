%% @author mbegejowicz <michalb@student.agh.edu.pl>, bzurkowski <zurkowski.bartosz@gmail.com>
%% @version 0.1
%% @doc This is a migration broker module. It handles migrations between islands and nodes

-module(mas_broker).

-include ("mas.hrl").

-export([init/0, local_migration/1, world_migration/1]).

%% ====================================================================
%% API
%% ====================================================================

init() ->
  net_adm:world().


local_migration(Agents) ->
  [mas_hybrid:send_agent(Agent) || Agent <- Agents].


world_migration(Agents) ->
  [send_to_node(Node, AgentsGroup) || {Node, AgentsGroup} <- group_agents(Agents)].


%% ====================================================================
%% Internal functions
%% ====================================================================

send_to_node(_, []) ->
  ok;

send_to_node(Node, Agents) ->
  spawn(Node, fun() -> mas_broker:local_migration(Agents) end).


group_agents(Agents) ->
  Nodes = nodes(),
  lists:zip([node() | Nodes], part(length(Nodes) + 1, Agents)).


part(1, List) ->
  [List];

part(2, List) ->
  {A, B} = lists:partition(fun(_) -> random:uniform(2) < 2 end, List),
  [A, B];

part(N, List) ->
  {Chunk, T} = lists:partition(fun(_) -> random:uniform(N) < 2 end, List),
  [Chunk | part(N - 1, T)].
