%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.2
%%% @doc The worker root resource, with links to all API endpoints.
%%% @end
%%%==========================================================

-module(erli_worker_root_res).

%% Webmachine Callbacks
-export([init/1,
	 allowed_methods/2,
	 options/2,
	 generate_etag/2,
	 content_types_provided/2,
	 as_json/2,
	 as_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

%%-----------------------------------------------------------
%% Webmachine Callbacks
%%-----------------------------------------------------------

-spec init(term()) -> {ok, [tuple()]}.
init(_Options) ->
    Data = jsx:encode([{<<"title">>, <<"erli API">>},
		       {<<"description">>,
			<<"simple REST API to the erli URL shortening service">>},
		       {<<"rels">>, [{<<"visits">>, <<"/visits/">>},
				     {<<"paths">>, <<"/paths/">>},
				     {<<"targets">>, <<"/targets">>}]}]),
    {ok, Data}.

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'OPTIONS'], RD, Ctx}.

options(RD, Ctx) ->
    {[{"Content-Length", "0"},
      {"Allow", "GET, HEAD, OPTIONS"}], RD, Ctx}.

generate_etag(RD, Ctx) ->
    Etag = mochihex:to_hex(erlang:md5(Ctx)),
    {Etag, RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"text/html", as_html},
      {"application/json", as_json}], RD, Ctx}.

as_json(RD, Ctx) ->
    {Ctx, RD, Ctx}.

as_html(RD, Ctx) ->
    {ok, Content} = index_dtl:render([]),
    {Content, RD, Ctx}.
