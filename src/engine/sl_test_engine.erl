%%
%%  @doc Dummy engine used for testing purposes
%%
-module( sl_test_engine ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-behaviour( sl_engine ).
-behaviour( gen_server ).
%% public interface
-export( [connect/2, prepare/3, query/2, execute/3] ).
%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2] ).

connect( _, _ ) ->
    { ok, Pid } = gen_server:start_link( ?MODULE, [], [] ),
    { ok, Pid }.

prepare( Pid, Statement, SQL ) ->
    gen_server:call( Pid, { prepare, Statement, SQL } ).

query( Pid, SQL ) ->
    gen_server:call( Pid, { query, SQL } ).

execute( Pid, Statement, Args ) ->
    gen_server:call( Pid, { execute, Statement, Args } ).

%%
%%	gen_server callbacks
%%
-record( state, {} ).

init( _ ) ->
	{ ok, #state{} }.

handle_call( { prepare, _Statement, _SQL }, _From, State ) ->
	{ reply, ok, State };

handle_call( { query, _SQL }, _From, State ) ->
    { reply, { ok, [#{ id => 1, data => "Hello" }, #{ id => 2, data => "World" }] }, State };

handle_call( { execute, insert, _Args }, _From, State ) ->
    { reply, ok, State };

handle_call( { execute, select, [ID1, ID2] }, _From, State ) ->
    { reply, { ok, [#{ id => ID1, data => <<"Hello">> }, #{ id => ID2, data => <<"World">> }] }, State };

handle_call( { execute, select, _Args }, _From, State ) ->
    { reply, { ok, [#{ id => 1, data => <<"Hello">> }, #{ id => 2, data => <<"World">> }] }, State };

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