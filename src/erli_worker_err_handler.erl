%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.2
%%% @doc The error handler for the API.
%%% @end
%%%==========================================================

-module(erli_worker_err_handler).

%% API
-export([render_error/3]).

%%-----------------------------------------------------------
%% API Methods
%%-----------------------------------------------------------

render_error(Code, Req, Reason) ->
    case Req:has_response_body() of
	{true, _} ->
	    Req:response_body();
	{false, _} ->
	    {ok, NReq} = Req:add_response_header("Content-Type",
						 "application/json"),
	    gen_body(Code, NReq, Reason)
    end.

%%----------------------------------------------------------
%% Internal Methods
%%----------------------------------------------------------

gen_body(400, Req, _Reason) ->
    {<<"{\"errors\":",
       "{\"message\":\"Malformed request\", \"status_code\":400}}">>,
     Req};
gen_body(404, Req, _Reason) ->
    {<<"{\"errors\":",
       "{\"message\":\"Resource not found\", \"status_code\":404}}">>,
     Req};
gen_body(405, Req, _Reason) ->
    {<<"{\"errors\":",
       "{\"message\":\"Method not allowed, use an OPTIONS request to",
       " find out what methods are supported by the resource.\", ",
       "\"status_code\":405}}">>,
     Req};
gen_body(410, Req, _Reason) ->
    {<<"{\"errors\":",
       "{\"message\":\"Resource gone. The resource identified by this URL",
       " has been permanetly removed.\", \"status_code\":410}}">>,
     Req};
gen_body(415, Req, _Reason) ->
    {<<"{\"errors\":",
       "{\"message\":\"Usupported media type, only application/json",
       " is supported.\", \"status_code\":415}}">>,
     Req};
gen_body(500, Req, Reason) ->
    webmachine_log:log_error(500, Req, Reason),
    error_logger:info_msg("error: ~s", [Reason]),
    {<<"{\"errors\":",
       "{\"message\":\"Internal server error, the error has been logged.\", ",
       "\"status_code\":500}}">>,
     Req};
gen_body(Code, Req, _Reason) ->
    {jsx:encode([{<<"errors">>, {<<"status_code">>, Code}}]),
     Req}.
