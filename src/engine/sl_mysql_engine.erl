-module( sl_mysql_engine ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [connect/1, prepare/3, query/2, execute/3] ).
-behaviour( sl_engine ).

-include_lib( "emysql/include/emysql.hrl" ).
-define( POOL_NAME, ?MODULE ).

connect( Args = #{  username    := Username, 
                    password    := Password,
                    database    := Database } ) ->
    mysql:start_link( [ { host, maps:get( host, Args, "localhost" ) }, 
                        { user, Username },
                        { password, Password }, 
                        { database, Database }] ).

prepare( Connection, Statement, SQL ) ->
    mysql:prepare( Connection, Statement, SQL ).

query( Connection, SQL ) ->
    case mysql:query( Connection, SQL ) of
        ok ->
            ok;
        { ok, ColumnNames, Rows } -> 
            { ok, lists:map( fun( Row ) -> maps:from_list( lists:zip( ColumnNames, Row ) ) end, Rows ) };
        { error, Reason } ->
            { error, Reason }
    end.

execute( Connection, Statement, Args ) ->
    case emysql:execute( Connection, Statement, Args ) of
        ok ->
            ok;
        { ok, ColumnNames, Rows } -> 
            { ok, lists:map( fun( Row ) -> maps:from_list( lists:zip( ColumnNames, Row ) ) end, Rows ) };
        { error, Reason } ->
            { error, Reason }
    end.