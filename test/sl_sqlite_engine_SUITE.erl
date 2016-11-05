-module( sl_sqlite_engine_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "common_test/include/ct.hrl" ).
-compile( export_all ).

groups()    -> [{ load_create_select_drop, [sequence], [load, create, insert, select, drop] }].
all()       -> [{ group, load_create_select_drop }].

init_per_suite( Config ) ->
    application:ensure_all_started( sequel ),
    { ok, Conn } = sl_sqlite_engine:connect( { undefined, 1 }, #{ path => ":memory:"} ),
    [{ conn, Conn } | Config ].

load( Config ) ->
    Dir = ?config( data_dir, Config ),
    { ok, [create, drop, insert, select] } = sl_query_map:load_dir( Dir ).

create( Config ) ->
    Conn = ?config( conn, Config ),
    { ok, _ } = sl_sqlite_engine:query( Conn, "CREATE TABLE sequel_test_table ( id INTEGER PRIMARY KEY ASC, data TEXT )" ).

insert( Config ) ->
    Conn = ?config( conn, Config ),
    "INSERT INTO sequel_test_table ( data ) VALUES ( ? )" = sl_query_map:get( insert ),
    { ok, Conn2 }   = sl_sqlite_engine:prepare( Conn, insert, sl_query_map:get( insert ) ),
    { ok, _ }       = sl_sqlite_engine:execute( Conn2, insert, [<<"Hello">>] ),
    { ok, _ }       = sl_sqlite_engine:execute( Conn2, insert, [<<"World">>] ).

select( Config ) ->
    Conn = ?config( conn, Config ),
    { ok, Conn2 }                               = sl_sqlite_engine:prepare( Conn, select, sl_query_map:get( select ) ),
    { ok, [#{ id := 1, data := <<"Hello">> }] } = sl_sqlite_engine:execute( Conn2, select, [1] ),
    { ok, [#{ id := 2, data := <<"World">> }] } = sl_sqlite_engine:execute( Conn2, select, [2] ).

drop( Config ) ->
    Conn = ?config( conn, Config ),
    sl_sqlite_engine:query( Conn, sl_query_map:get( drop ) ).

end_per_suite( _Config ) ->
    application:stop( sequel ).