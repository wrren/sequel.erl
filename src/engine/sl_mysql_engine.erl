-module( sl_mysql_engine ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [connect/1, schema/1, query/3] ).
-behaviour( sl_engine ).

-include_lib( "emysql/include/emysql.hrl" ).

connect( #{ connections := Connections, 
            username    := Username, 
            password    := Password,
            database    := Database } ) ->
    case emysql:add_pool( sequel_pool, [    { size, Connections },
                                            { user, Username },
                                            { password, Password },
                                            { database, Database }] ) of
        { reply, { error, Reason }, _ } ->
            { error, Reason };
        { reply, ok, _ } ->
            { ok, { pool, sequel_pool } }
    end.

schema( { pool, PoolName } ) ->
    case emysql:execute( PoolName, <<"SHOW TABLES">> ) of
