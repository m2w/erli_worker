%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.2
%%% @doc The erli_api_worker application module.
%%% @end
%%%==========================================================

-module(erli_api_worker_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%-----------------------------------------------------------
%% Application Callbacks
%%-----------------------------------------------------------

start(_StartType, _StartArgs) ->
    case erli_api_worker_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.
