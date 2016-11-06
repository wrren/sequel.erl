-module( sequel ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [connect/1, connect/2, query/1, query/2, prepare/2, prepare/3, execute/2, execute/3] ).

%%  Default Connection Pool
-define( DEFAULT_POOL, default_pool ).

%%
%%  @doc Connect to the database using the provided arguments. This function will create
%%  a connection pool with the default name.
%%
connect( Args ) ->
    connect( ?DEFAULT_POOL, Args ).

%%
%%  @doc Connect to the database using the provided arguments. This function will create
%%  a connection pool with the specified name.
%%
connect( PoolName, Args = #{ engine := Engine } ) when is_atom( PoolName ) ->
    case Engine of
        sqlite      -> connect( PoolName, sl_sqlite_engine, Args );
        postgres    -> connect( PoolName, sl_postgres_engine, Args );
        test        -> connect( PoolName, sl_test_engine, Args );
        _           -> { error, undefined_db_engine }
    end.

%%
%%  @doc Create a new connection pool and connect using the provided arguments
%%
connect( PoolName, EngineMod, Args ) when is_atom( PoolName ), is_atom( EngineMod ), is_map( Args ) ->
    sl_worker_pool:add_pool( PoolName, [    { name, { local, PoolName } },
                                            { worker_module, sl_worker },
                                            { size, maps:get( pool_size, Args, 20 ) },
                                            { max_overflow, maps:get( max_overflow, Args, 20 ) } ], [EngineMod, Args] ).

%%
%%  @doc Prepare the given query and associate it with the provided prepared statement name
%%
prepare( Statement, SQL ) when is_atom( Statement ), is_list( SQL ) ->
    prepare( ?DEFAULT_POOL, Statement, SQL ).

%%
%%  @doc Prepare the given query and associate it with the provided prepared statement name
%%
prepare( PoolName, Statement, SQL ) when is_atom( PoolName ), is_atom( Statement ), is_list( SQL ) ->
    sl_statement_map:put( PoolName, Statement, SQL ),
    poolboy:transaction( PoolName, fun( Worker ) ->
        sl_worker:prepare( Worker, Statement, SQL )
    end ).

%%
%%  @doc Execute a raw query against the database on the default pool
%%
query( SQL ) ->
    query( ?DEFAULT_POOL, SQL ).

%%
%%  @doc Execute a raw query against the database on the specified pool
%%
query( PoolName, SQL ) ->
    poolboy:transaction( PoolName, fun( Worker ) ->
        sl_worker:query( Worker, SQL )
    end ).

%%
%%  @doc Execute a prepared statement on the default pool
%%
execute( Statement, Args ) ->
    execute( ?DEFAULT_POOL, Statement, Args ).

%%
%%  @doc Execute a prepared statement on the specified pool. If the statement has yet to be prepared and
%%  no query has been loaded into the query map with a matching name, the execution will fail.
%%
%%  If the statement hasn't been prepared, but a query has been loaded into the query map with the same
%%  name, the query SQL will be prepared and then executed.
%%
execute( PoolName, Statement, Args ) ->
    case { sl_statement_map:get( PoolName, Statement ), sl_query_map:get( Statement ) } of
        { undefined, undefined } ->
            { error, undefined_statement };
        { undefined, SQL } ->
            poolboy:transaction( PoolName, fun( Worker ) ->
                sl_worker:prepare( Worker, Statement, SQL ),
                sl_worker:execute( Worker, Statement, Args )
            end );
        { SQL, _ } ->
            poolboy:transaction( PoolName, fun( Worker ) ->
                sl_worker:prepare( Worker, Statement, SQL ),
                sl_worker:execute( Worker, Statement, Args )
            end )
    end.