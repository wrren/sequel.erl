-module( sl_statement_map ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-behaviour( gen_server ).
%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2] ).
%% public interface
-export( [start_link/0, stop/0, get/2, put/3] ).

%%
%%	@doc Start the statement map
%%
-spec start_link() -> { ok, pid() }.
start_link() ->
	gen_server:start_link( { local, ?MODULE }, ?MODULE, [], [] ).

%%
%%	@doc Stop the statement map
%%
-spec stop() -> ok.
stop() ->
	gen_server:stop( ?MODULE ).

%%
%%	@doc Get the query string corresponding to the given statement name
%%
-spec get( atom(), atom() ) -> undefined | string().
get( PoolName, Statement ) ->
	case ets:lookup( ?MODULE, { PoolName, Statement } ) of
		[{ { PoolName, Statement }, SQL }]	-> SQL;
		_ 					                -> undefined
	end.

%%
%%	@doc Add a query string and associate it with the given statement name
%%
-spec put( atom(), atom(), string() ) -> ok.
put( PoolName, Statement, SQL ) when is_list( SQL ) ->
    case ets:lookup( ?MODULE, { PoolName, Statement } ) of
		[{ { PoolName, Statement }, _SQL }]	    -> ok;
		_ 					                    -> gen_server:call( ?MODULE, { put, PoolName, Statement, SQL } )
	end.

%%
%%	gen_server callbacks
%%
-record( state, { table = ets:new( ?MODULE, [named_table, protected, { read_concurrency, true }] ) } ).

init( _ ) ->
	{ ok, #state{} }.

handle_call( { put, PoolName, Statement, SQL }, _From, State = #state{ table = Table } ) ->
	{ reply, ets:insert( Table, { { PoolName, Statement }, SQL } ), State };

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