-module( sl_postgres_engine_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "common_test/include/ct.hrl" ).
-compile( export_all ).

groups()    -> [{ load_create_select_drop, [sequence], [load, create, insert, select, drop] }].
all()       ->
    case ct:get_config( postgres_connect ) of
        undefined ->
            { skip, "PostgreSQL Configuration undefined in config/test.config" };
        _ ->
            [{ group, load_create_select_drop }]
    end.

init_per_suite( Config ) ->
    application:ensure_all_started( sequel ),
    Config.

load( Config ) ->
    Dir = ?config( data_dir, Config ),
    { ok, [create, drop, insert, select] } = sl_query_map:load_dir( Dir ).

create( _Config ) ->
    { ok, Conn } = sl_postgres_engine:connect( { undefined, 1 }, ct:get_config( postgres_connect ) ),
    { ok, _ } = sl_postgres_engine:query( Conn, sl_query_map:get( create ) ).

insert( _Config ) ->
    { ok, Conn } = sl_postgres_engine:connect( { undefined, 1 }, ct:get_config( postgres_connect ) ),
    { ok, Conn2 }   = sl_postgres_engine:prepare( Conn, insert, sl_query_map:get( insert ) ),
    { ok, _ }       = sl_postgres_engine:execute( Conn2, insert, [<<"Hello">>] ),
    { ok, _ }       = sl_postgres_engine:execute( Conn2, insert, [<<"World">>] ).

select( _Config ) ->
    { ok, Conn } = sl_postgres_engine:connect( { undefined, 1 }, ct:get_config( postgres_connect ) ),
    { ok, Conn2 }                               = sl_postgres_engine:prepare( Conn, select, sl_query_map:get( select ) ),
    { ok, [#{ id := 1, data := <<"Hello">> }] } = sl_postgres_engine:execute( Conn2, select, [1] ),
    { ok, [#{ id := 2, data := <<"World">> }] } = sl_postgres_engine:execute( Conn2, select, [2] ).

drop( _Config ) ->
    { ok, Conn } = sl_postgres_engine:connect( { undefined, 1 }, ct:get_config( postgres_connect ) ),
    sl_postgres_engine:query( Conn, sl_query_map:get( drop ) ).

end_per_suite( _Config ) ->
    application:stop( sequel ).