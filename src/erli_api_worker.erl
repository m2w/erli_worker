%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.2
%%% @doc Management API for the erli API worker.
%%% @end
%%%==========================================================

-module(erli_api_worker).

-export([start/0,
	 start_link/0,
	 stop/0]).

%%-----------------------------------------------------------
%% API Functions
%%-----------------------------------------------------------

%% @doc Start the worker for inclusion in a supervisor tree
%% Expects the supervisor to take care of mnesia initialization!
-spec start_link() -> {ok, pid()}.
start_link() ->
    ok = ensure_deps_started(),
    erli_api_worker_sup:start_link().

-spec start() -> ok.
start() ->
    ok = ensure_deps_started(),
    application:load(erli_api_worker), %% force loading of .config
    case application:get_env(erli_api_worker, unmanaged) of
	{ok, true} ->
	    %% if running an "unmanaged" worker, instantiate mnesia locally
	    erli_storage:create_tables();
	undefined ->
	    ok
    end,
    application:start(erli_api_worker).


-spec stop() -> ok.
stop() ->
    Res = application:stop(erli_api_worker),
    application:stop(egeoip),
    application:stop(mnesia),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(inets),
    Res.

%%-----------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------

ensure_deps_started() ->
    ensure_started(inets),
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(webmachine),
    ensure_started(mnesia),
    ensure_started(egeoip).


ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
	{error, {already_started, App}} ->
            ok
    end.
