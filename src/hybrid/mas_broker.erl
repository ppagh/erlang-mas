%% @author mbegejowicz <michalb@student.agh.edu.pl>
%% @version 0.1
%% @doc This is a migration broker module. It handles migrations between islands and nodes

-module(mas_broker).

-include ("mas.hrl").

%% API
-export([send_agents/1, migrate_agents/1, init/0]).

init() ->
  [{Node, net_adm:ping(Node)} || Node <- net_adm:host_file()].

send_agents(Agents) ->
  [send_to_node(Node, NodeAgents) || {Node, NodeAgents} <- group_agents(Agents)],
  ok.

migrate_agents(Agents) ->
  [mas_hybrid:sendAgent(Agent) || Agent <- Agents].

send_to_node(_, []) ->
  ok;

send_to_node(Node, Agents) ->
  spawn(Node, fun() -> mas_broker:migrate_agents(Agents) end).

group_agents(Agents) ->
  lists:zip([node() | nodes()], part(length(nodes()) + 1, Agents)).

part(1, List) ->
  [List];

part(2, List) ->
  {A, B} = lists:partition(fun(_) -> random:uniform(2) < 2 end, List),
  [A, B];

part(N, List) ->
  {Chunk, T} = lists:partition(fun(_) -> random:uniform(N) < 2 end, List),
  [Chunk | part(N - 1, T)].
