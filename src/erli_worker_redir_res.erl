%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.2
%%% @doc The resource responsible for client-facing redirects.
%%% @end
%%%==========================================================

-module(erli_worker_redir_res).

-export([init/1,
	 allowed_methods/2,
	 options/2,
	 resource_exists/2,
	 previously_existed/2,
	 content_types_provided/2,
	 redir/2]).

-include_lib("include/erli_models.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%-----------------------------------------------------------
%% Webmachine Callbacks
%%-----------------------------------------------------------
init(_Opts) ->
    {ok, {}}.

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'OPTIONS'], RD, Ctx}.

options(RD, Ctx) ->
    {[{"Content-Length", "0"},
      {"Allow", "GET, HEAD, OPTIONS"}], RD, Ctx}.

resource_exists(RD, Ctx) ->
    Id = list_to_binary(wrq:path_info(id, RD)),
    case erli_storage:read(path, Id) of
	{error, _Error} ->
	    {false, RD, Ctx};
	Path ->
	    {not Path#path.is_banned, RD, Path}
    end.

previously_existed(RD, Ctx) when is_record(Ctx, path)->
    {Ctx#path.is_banned, RD, Ctx};
previously_existed(RD, Ctx) ->
    {false, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", redir},
      {"text/html", redir}], RD, Ctx}.

redir(RD, Ctx) ->
    case wrq:get_qs_value("landing", RD) of
	undefined -> % immediately redirect
	    Target = erli_storage:read(target, Ctx#path.target_id),
	    RD1 = wrq:set_resp_header("Cache-Control",
				      "max-age=86400, must-revalidate", RD),
	    RD2 = wrq:set_resp_header("Location",
				      binary_to_list(Target#target.url), RD1),
	    {{halt, 301}, RD2, Ctx};
	_Val -> % redirect to details page
	    RD1 = wrq:set_resp_header("Cache-Control",
				      "max-age=86400, must-revalidate", RD),
	    RD2 = wrq:set_resp_header("Location",
				      "/paths/" ++ binary_to_list(Ctx#path.id),
				      RD1),
	    {{halt, 303}, RD2, Ctx}
    end.
