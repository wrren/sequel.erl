-module( sl_sqlite_engine ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [connect/2, prepare/3, query/2, execute/3] ).
-behaviour( sl_engine ).

-record( state, {   connection  :: esqlite3:connection(),
                    statements  :: #{ atom() := esqlite3:statement() }
} ).

connect( { _, 1 }, #{ path := DBPath } ) ->
    case esqlite3:open( DBPath ) of
        { ok, Conn } ->
            { ok, #state{ connection = Conn, statements = maps:new() } };
        { error, Reason } ->
            { error, Reason }
    end.


prepare( State = #state{ connection = Conn, statements = S }, ID, SQL ) ->
    case esqlite3:prepare( SQL, Conn ) of
        { ok, Statement } ->
            { ok, State#state{ statements = maps:put( ID, Statement, S ) } };
        { error, Reason } ->
            { error, Reason }
    end.

query( #state{ connection = Conn }, SQL ) ->
    case esqlite3:prepare( SQL, Conn ) of
        { ok, Statement } ->
            execute( Statement, [] );
        { error, Reason } ->
            { error, Reason }
    end.

execute( #state{ statements = S }, ID, Args ) ->
    case maps:get( ID, S, undefined ) of
        undefined ->
            { error, { undefined_statement, ID } };
        Statement ->
            execute( Statement, Args )
    end.

execute( Statement, [] ) ->
    case esqlite3:fetchall( Statement ) of
        Rows when is_list( Rows ) ->
            ColumnNames = tuple_to_list( esqlite3:column_names( Statement ) ),
            { ok, lists:map( fun( Row ) -> maps:from_list( lists:zip( ColumnNames, tuple_to_list( Row ) ) ) end, Rows ) };
        Error ->
            { error, Error }
    end;

execute( Statement, Args ) ->
    esqlite3:bind( Statement, Args ),
    execute( Statement, [] ). 
