%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.2
%%% @doc A resource to serve static content.
%%% This is based on the webmachine_demo_fs_resource in the
%%% webmachine source.
%%% @end
%%%==========================================================

-module(erli_worker_static_res).

-export([init/1,
	 allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         generate_etag/2,
	 provide_content/2]).

-record(state, {basedir=[], fpath, content=undefined}).

-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(_Opts) ->
    StaticDir = erli_worker_utils:get_env(static_dir),
    {ok, App} = application:get_application(),
    BaseDir = filename:join(code:priv_dir(App), StaticDir),
    {ok, #state{basedir = BaseDir}}.

allowed_methods(RD, Ctx) ->
    {['GET'], RD, Ctx}.

resource_exists(RD, Ctx) ->
    Path = wrq:disp_path(RD),
    case file_exists(Ctx, Path) of
	{true, FPath} ->
	    {true, RD, Ctx#state{fpath=FPath}};
	_ ->
	    {false, RD, Ctx}
    end.

content_types_provided(RD, Ctx) ->
    CT = webmachine_util:guess_mime(wrq:disp_path(RD)),
    {[{CT, provide_content}], RD, Ctx}.


generate_etag(RD, Ctx) ->
    case Ctx#state.content of
	undefined ->
	    Content = fetch_content(RD, Ctx),
            ETag = hash(Content),
	    {ETag, RD, Ctx#state{content=Content}};
	Content ->
            ETag = hash(Content),
            {ETag, RD, Ctx}
    end.

provide_content(RD, Ctx) ->
    case Ctx#state.content of
	undefined ->
	    Content = fetch_content(RD, Ctx),
	    {Content, RD, Ctx};
	Content ->
	    {Content, RD, Ctx}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

hash(Content) ->
    mochihex:to_hex(binary_to_list(crypto:hash(sha, Content))).

fetch_content(_RD, Ctx) ->
    {ok, Content} = file:read_file(Ctx#state.fpath),
    Content.

file_path(_Ctx, []) ->
    false;
file_path(Ctx, Name) ->
    RelName = case hd(Name) of
        "/" -> tl(Name);
        _ -> Name
    end,
    filename:join([Ctx#state.basedir, RelName]).

file_exists(Ctx, Name) ->
    FPath = file_path(Ctx, Name),
    case filelib:is_regular(FPath) of
        true ->
            {true, FPath};
        false ->
            false
    end.
