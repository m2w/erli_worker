%%%==========================================================
%%% @author Moritz Windelen
%%% @version 0.2
%%% @doc A mnesia-based backend for erli_storage.
%%% @end
%%%==========================================================

-module(erli_storage).

%% erli_storage_backend callbacks
-export([write/1,
	 read/2,
	 read_bulk/2,
	 delete/1,
	 count/1]).

%% Management API
-export([create_tables/0]).

-include("include/erli_models.hrl").

%%-----------------------------------------------------------
%% API Functions
%%-----------------------------------------------------------

-spec write(object()) -> object() | conflict_error() | mnesia_error().
write(Obj) when is_record(Obj, target) ->
    RecordNumber = mnesia:dirty_update_counter(counters, target, 1),
    LastModified = erli_api_worker_utils:unix_timestamp(),
    Object = Obj#target{record_number=RecordNumber, last_modified=LastModified},
    case mnesia:dirty_index_read(targets, Object#target.url, url) of
	[] ->
	    case generate_id(targets) of
		{error, Error} ->
		    {error, Error};
		Id ->
		    UpdatedObject = Object#target{id=Id},
		    ok = mnesia:dirty_write(targets, UpdatedObject),
		    UpdatedObject
	    end;
	[ConflictingRecord] ->
	    {error, {conflict,
		     {Object#target{id= <<"none">>}, ConflictingRecord}}}
    end;
write(Obj) when is_record(Obj, path) ->
    RecordNumber = mnesia:dirty_update_counter(counters, path, 1),
    Object = Obj#path{record_number=RecordNumber},
    case read(target, Object#path.target_id) of
	{error, _Error} ->
	    {error, no_matching_target};
	_T ->
	    case Object#path.id of
		undefined ->
		    case generate_id(paths) of
			{error, Error} ->
			    {error, Error};
			Id ->
			    UpdatedObject = Object#path{id=Id},
			    ok = mnesia:dirty_write(paths, UpdatedObject),
			    UpdatedObject
		    end;
		Id ->
		    case read(path, Id) of
			{error, not_found} ->
			    ok = mnesia:dirty_write(paths, Object),
			    Object;
			{error, Error} ->
			    {error, Error};
			ConflictingObject ->
			    {error, {conflict,
				     {Object, ConflictingObject}}}
		    end
	    end
    end;
write(Obj) when is_record(Obj, visit) ->
    Id = mnesia:dirty_update_counter(counters, visit, 1),
    Time = erli_api_worker_utils:unix_timestamp(),
    UpdatedObject = Obj#visit{id=erli_api_worker_utils:int_to_bitstring(Id),
			      record_number=Id, time=Time},
    ok = mnesia:dirty_write(visits, UpdatedObject),
    UpdatedObject.

-spec read(object_type(), id()) -> object() | mnesia_error().
read(target, Id) ->
    wrap_read(mnesia:dirty_read(targets, Id));
read(path, Id) ->
    wrap_read(mnesia:dirty_read(paths, Id));
read(visit, Id) ->
    wrap_read(mnesia:dirty_read(visits, Id)).

-spec read_bulk(collection_type(), range()) -> collection().
read_bulk(targets, {Start, End}) ->
    mnesia:dirty_select(targets, [{#target{record_number='$1', _='_'},
				   [{'>=', '$1', Start},
				    {'=<', '$1', End}],
				   ['$_']}]);
read_bulk(paths, {Start, End}) ->
    mnesia:dirty_select(paths, [{#path{record_number='$1', _='_'},
				 [{'>=', '$1', Start},
				  {'=<', '$1', End}],
				 ['$_']}]);
read_bulk(visits, {Start, End}) ->
    mnesia:dirty_select(visits, [{#visit{record_number='$1', _='_'},
				  [{'>=', '$1', Start},
				   {'=<', '$1', End}],
				  ['$_']}]).

-spec delete(object()) ->
		    {request_accepted | target_banned, object()}.
delete(Object) when is_record(Object, target) ->
    CurrentLimit = erli_api_worker_utils:get_env(flag_limit),
    LastModified = erli_api_worker_utils:unix_timestamp(),

    case Object#target.flag_count of
	FC when FC < CurrentLimit ->
	    UpdatedObject = Object#target{flag_count=FC+1,
					  last_modified=LastModified},
	    mnesia:dirty_write(targets, UpdatedObject),
	    {request_accepted, UpdatedObject};
	FC when FC >= CurrentLimit ->
	    UpdatedObject = Object#target{last_modified=LastModified,
					  flag_count=FC+1,
					  is_banned=true},
	    ban(UpdatedObject),
	    {target_banned, UpdatedObject}
    end;
delete(Object) when is_record(Object, path) ->
    Target = read(target, Object#path.target_id),
    delete(Target).

-spec count(collection_type()) -> collection_size().
count(Collection) ->
    mnesia:table_info(Collection, size).

-spec wrap_read(list() | {aborted, atom()}) -> object() | mnesia_error().
wrap_read([]) ->
    {error, not_found};
wrap_read([Record]) ->
    Record;
wrap_read({aborted, Error}) ->
    {error, Error}.

-spec ban(object()) -> {atomic, integer()} | mnesia_error().
ban(Target) when is_record(Target, target) ->
    mnesia:transaction(
      fun() ->
	      AffectedPaths =
		  mnesia:select(paths, [{#path{target_id='$1', _='_'},
					[{'=:=', '$1', Target#target.id}],
					['$_']}]),
	      lists:map(fun(Path) ->
				UpdatedPath = Path#path{is_banned=true},
				mnesia:write(paths, UpdatedPath, write)
			end, AffectedPaths),
	      mnesia:write(targets, Target, write),
	      length(AffectedPaths)
      end).

-spec generate_id(targets | paths) -> id() | {error, unable_to_generate_id}.
generate_id(Table) ->
    generate_id(Table, 0).

-spec generate_id(targets | paths, integer()) ->
			 id() |
			 {error, unable_to_generate_id}.
generate_id(Table, Attempts) when Attempts < 20 ->
    Id = re:replace(
	   base64:encode(
	     crypto:rand_bytes(3)),
	   "[\/+=]", "",
	   [global, {return, binary}]),
    case mnesia:dirty_read(Table, Id) of
	[] -> Id;
	_Record -> generate_id(Table, Attempts+1)
    end;
generate_id(_Table, _Attempts) ->
    {error, unable_to_generate_id}.

%%-----------------------------------------------------------
%% Management API
%%-----------------------------------------------------------

%% TODO: TERRIBLE COPY AND PASTE REFACTOR INTO MANAGER AND SHARE SOMEHOW BETWEEN WORKER AND MANAGER
create_tables() ->
    mnesia:create_table(counters, [{attributes, [type, id]}]),
    mnesia:create_table(targets, [{record_name, target},
				 {index, [url, record_number]},
				 {attributes, record_info(fields, target)}]),
    mnesia:create_table(paths, [{record_name, path},
			       {index, [target_id, record_number]},
			       {attributes, record_info(fields, path)}]),
    mnesia:create_table(visits, [{record_name, visit},
				{index, [path_id]},
				{attributes, record_info(fields, visit)}]).
