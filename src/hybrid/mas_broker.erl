%% @author mbegejowicz <michalb@student.agh.edu.pl>
%% @version 0.1
%% @doc This is a migration broker module. It handles migrations between islands and nodes

-module(mas_broker).

-include ("mas.hrl").

-type agent() :: mas:agent().

%% API
-export([send_agents/1]).

-spec send_agents([agent()]) -> ok.
send_agents(Agents) ->
  [mas_hybrid:sendAgent(Agent) || Agent <- Agents],
  ok.
