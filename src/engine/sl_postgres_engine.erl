-module( sl_postgres_engine ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [connect/2, prepare/3, query/2, execute/3] ).
-behaviour( sl_engine ).

-include_lib( "epgsql/include/epgsql.hrl" ).

connect( { _PoolID, _Size }, Args = #{   database := Database,
                                        username := Username,
                                        password := Password } ) ->
    case epgsql:connect(    maps:get( host, Args, "localhost" ),
                            Username, Password,
                            maps:fold( fun  ( timeout, Timeout, Opts ) ->
                                                [{ timeout, Timeout } | Opts];
                                            ( port, Port, Opts ) ->
                                                [{ port, Port } | Opts];
                                            ( ssl, IsEnabled, Opts ) ->
                                                [{ ssl, IsEnabled } | Opts];
                                            ( _, _, Opts ) ->
                                                Opts
                                            end, [{ database, Database }], Args ) ) of
        { ok, Conn } ->
            { ok, Conn };
        { error, Reason } ->
            { error, Reason }
    end.


prepare( Conn, ID, SQL ) ->
    case epgsql:parse( Conn, atom_to_list( ID ), SQL, [] ) of
        { ok, _Statement } ->
            { ok, Conn };
        { error, Reason } ->
            { error, Reason }
    end.

query( Conn, SQL ) ->
    case epgsql:squery( Conn, SQL ) of
        { ok, Columns, Rows } ->
            { ok, result_map( Columns, Rows ) };
        { ok, Count } ->
            { ok, Count };
        { ok, _Count, Columns, Rows } ->
            { ok, result_map( Columns, Rows ) };
        { error, Reason } ->
            { error, Reason }
    end.

execute( Conn, ID, Args ) ->
    case epgsql:prepared_query( Conn, atom_to_list( ID ), Args ) of
        { ok, Columns, Rows } ->
            { ok, result_map( Columns, Rows ) };
        { ok, Count } ->
            { ok, Count };
        { ok, _Count, Columns, Rows } ->
            { ok, result_map( Columns, Rows ) };
        { error, Reason } ->
            { error, Reason }
    end.

result_map( Columns, Rows ) ->
    ColumnNames = [binary_to_atom( Name, utf8 ) || #column{ name = Name } <- Columns],
    lists:map( fun( Row ) -> maps:from_list( lists:zip( ColumnNames, tuple_to_list( Row ) ) ) end, Rows ).