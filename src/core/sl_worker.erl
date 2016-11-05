-module( sl_worker ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-behaviour( gen_server ).
%% gen_server callbacks
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2] ).
%% public interface
-export( [start_link/1, stop/1, prepare/3, query/2, execute/3] ).

%%
%%  @doc Start a worker process
%%
-spec start_link( term() ) -> { ok, pid() }.
start_link( Args ) ->
    gen_server:start_link( ?MODULE, Args, [] ).

%%
%%  @doc Stop a worker process
%%
stop( Pid ) ->
    gen_server:stop( Pid ).

-spec prepare( pid(), atom(), string() ) -> ok | { error, term() }.
prepare( Worker, Statement, SQL ) ->
    gen_server:call( Worker, { prepare, Statement, SQL } ).

-spec query( pid(), string() ) -> { ok, sl_engine:result() } | { error, term() }.
query( Worker, SQL ) ->
    gen_server:call( Worker, { query, SQL } ).

-spec execute( pid(), atom(), [term()] ) -> { ok, sl_engine:result() } | { error, term() }.
execute( Worker, Statement, Args ) ->
    gen_server:call( Worker, { execute, Statement, Args } ).

-record( state, {
    connection      :: term(),
    engine          :: module(),
    statements      :: [atom()]
} ).

init( [Pool, EngineMod, Args] ) ->
    case EngineMod:connect( Pool, Args ) of
        { ok, Connection } ->
            { ok, #state{ connection = Connection, engine = EngineMod, statements = [] } };
        { error, Reason } ->
            { stop, Reason }
    end.

handle_call( { prepare, Statement, SQL }, _From, State = #state{ connection = Connection, engine = EngineMod, statements = Statements } ) ->
    case lists:member( Statement, Statements ) of
        true    -> 
            { reply, ok, State };
        false   ->
            case EngineMod:prepare( Connection, Statement, SQL ) of
                { ok, NewConnection } ->
                    { reply, ok, State#state{ connection = NewConnection, statements = [Statement | Statements] } };
                { error, Reason } ->
                    { reply, { error, Reason }, State }
            end
    end;

handle_call( { execute, Statement, Args }, _From, State = #state{ connection = Connection, engine = EngineMod } ) ->
    { reply, EngineMod:execute( Connection, Statement, Args ), State };

handle_call( { query, SQL }, _From, State = #state{ connection = Connection, engine = EngineMod } ) when is_atom( SQL ) ->
    case sl_query_map:get( SQL ) of
        undefined ->
            { reply, { error, undefined_query }, State };
        SQLText ->
            { reply, EngineMod:query( Connection, SQLText ), State }
    end;

handle_call( { query, SQL }, _From, State = #state{ connection = Connection, engine = EngineMod } ) when is_list( SQL ) ->
    { reply, EngineMod:query( Connection, SQL ), State };

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