%% @author krzywick
%% @doc @todo Add description to config.

-module(config).

%% ====================================================================
%% API functions
%% ====================================================================
-compile(export_all).

stopPrec() -> 0.001.
populationSize() -> 100.
initialEnergy() -> 10.
%steps() -> 5000.

reproductionThreshold() -> 11.
reproductionTransfer() -> 5.
fightTransfer() -> 10.

problemSize() -> 10.
mutationRate() -> 0.1.
mutationRange() -> 0.05.
mutationChance() -> 0.75.
recombinationChance() -> 0.3.
migrationProbability() -> 0.001.

timeout() -> 1000.

%% ====================================================================
%% Internal functions
%% ====================================================================
