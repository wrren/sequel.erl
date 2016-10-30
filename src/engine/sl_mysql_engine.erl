-module( sl_mysql_engine ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [connect/2, prepare/3, query/2, execute/3] ).
-behaviour( sl_engine ).

-include_lib( "emysql/include/emysql.hrl" ).
-define( POOL_NAME, ?MODULE ).

connect( { PoolId, Size }, Args = #{  username    := Username, 
                            password    := Password,
                            database    := Database } ) ->
    case emysql:add_pool( PoolId, [ { host, maps:get( host, Args, "localhost" ) },
                                    { port, maps:get( port, Args, 3306 ) },
                                    { encoding, maps:get( encoding, Args, utf8 ) },
                                    { size, Size },
                                    { user, Username },
                                    { password, Password },
                                    { database, Database }] ) of
        ok                              -> { ok, { pool, PoolId } };
        { error, pool_already_exists }  -> { ok, { pool, PoolId } }
    end.

prepare( _Connection, Statement, SQL ) ->
    ok = emysql:prepare( Statement, list_to_binary( SQL ) ).

query( { pool, PoolId }, SQL ) ->
    result( emysql:execute( PoolId, SQL ) ).

execute( { pool, PoolId }, Statement, Args ) ->
    result( emysql:execute( PoolId, Statement, Args ) ).

result( #ok_packet{} ) ->
    ok;

result( #result_packet{ field_list = ColumnNames, rows = Rows } ) -> 
    { ok, lists:map( fun( Row ) -> maps:from_list( lists:zip( ColumnNames, Row ) ) end, Rows ) };

result( #error_packet{ msg = Reason } ) ->
    { error, Reason }.