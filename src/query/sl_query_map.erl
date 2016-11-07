-module( sl_query_map ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-behaviour( gen_server ).
%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2] ).
%% public interface
-export( [start_link/0, stop/0, get/1, put/2, load_dir/1, load_file/1, load_file/2] ).

-type query_id() :: atom() | { atom(), atom() }.

%%
%%	@doc Start the query map
%%
-spec start_link() -> { ok, pid() }.
start_link() ->
	gen_server:start_link( { local, ?MODULE }, ?MODULE, [], [] ).

%%
%%	@doc Stop the query map
%%
-spec stop() -> ok.
stop() ->
	gen_server:stop( ?MODULE ).

%%
%%	@doc Get the query string corresponding to the given query ID atom
%%
-spec get( query_id() ) -> undefined | string().
get( Query ) ->
	case ets:lookup( ?MODULE, Query ) of
		[{ Query, SQL }]	-> SQL;
		_ 					-> undefined
	end.

%%
%% 	@doc Enumerate all SQL files in the given directory and generate ID->SQL mappings where the 
%%	ID for a given query is the atom form of the SQL file name minus the file extension.
%%
-spec load_dir( string() ) -> { ok, [query_id()] } | { error, term() }.
load_dir( Path ) when is_list( Path ) ->
	case file:list_dir( Path ) of
		{ ok, Filenames } ->
			{ ok, lists:usort( lists:filtermap( fun( Filename ) ->
				case load_file( filename:join( Path, Filename ) ) of
					{ ok, Query } 		-> { true, Query };
					{ error, _Reason }	-> false
				end end, Filenames ) ) };
		{ error, Reason } ->
			{ error, Reason }
	end.

%%
%%	@doc Load the SQL contained in the given file into the query map. The query ID for the
%%	SQL read from the file will be the file basename, stripped of its extension, converted to an atom
%%
-spec load_file( string() ) -> { ok, query_id() } | { error, term() }.
load_file( Path ) when is_list( Path ) ->
	load_file( Path, list_to_atom( filename:rootname( filename:basename( Path ) ) ) ).

%%
%%	@doc Load the SQL contained in the given file into the query map and associate it with the 
%%	given ID
%%
-spec load_file( string(), query_id() ) -> { ok, query_id() } | { error, term() }.
load_file( Path, QueryID ) when is_list( Path ) ->
	case file:read_file( Path ) of
		{ ok, Binary } ->
			?MODULE:put( QueryID, binary_to_list( Binary ) ),
			{ ok, QueryID };
		{ error, Reason } ->
			{ error, Reason }
	end.

%%
%%	@doc Add a query string and associate it with the given query ID atom
%%
-spec put( atom(), string() ) -> ok.
put( Query, SQL ) when is_list( SQL ) ->
	gen_server:call( ?MODULE, { put, Query, SQL } ).

%%
%%	gen_server callbacks
%%
-record( state, { table = ets:new( ?MODULE, [named_table, protected, { read_concurrency, true }] ) } ).

init( _ ) ->
	{ ok, #state{} }.

handle_call( { put, Query, SQL }, _From, State = #state{ table = Table } ) ->
	{ reply, ets:insert( Table, { Query, SQL } ), State };

handle_call( _Call, _From, State ) ->
	{ reply, ok, State }.

handle_cast( _Cast, State ) ->
	{ noreply, State }.

handle_info( _Info, State ) ->
	{ noreply, State }.

code_change( _OldVersion, State, _Extra ) ->
	{ ok, State }.

terminate( _Reason, _State ) ->
	ok.