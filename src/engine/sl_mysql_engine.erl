-module( sl_mysql_engine ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [connect/2, prepare/3, query/2, execute/3] ).
-behaviour( sl_engine ).

connect( PoolId, Args = #{  username    := Username, 
                            password    := Password,
                            database    := Database } ) ->
    case mysql:start_link(      PoolId, maps:get( host, Args, "localhost" ), 
                                maps:get( port, Args, undefined ), Username, Password, Database ) of
        { ok, _Pid }        -> { ok, PoolId };
        { error, Reason }   -> { error, Reason }
    end.

prepare( _Connection, Statement, SQL ) ->
    mysql:prepare( Statement, SQL ).

query( Connection, SQL ) ->
    result( mysql:query( Connection, SQL ) ).

execute( Connection, Statement, Args ) ->
    result( mysql:execute( Connection, Statement, Args ) ).

result( { updated, _Res } ) -> ok;

result( { data, Res } ) ->
    ColumnNames = [Name || { _Table, _Field, _Length, Name } <- mysql:get_result_field_info( Res ) ],
    { ok, lists:map( fun( Row ) -> maps:from_list( lists:zip( ColumnNames, Row ) ) end, mysql:get_result_rows( Res ) ) };

result( { error, Reason } ) ->
    { error, mysql:get_result_reason( Reason ) }.