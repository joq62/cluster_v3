%%%-------------------------------------------------------------------
%% @doc cluster public API
%% @end
%%%-------------------------------------------------------------------

-module(cluster_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cluster_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
