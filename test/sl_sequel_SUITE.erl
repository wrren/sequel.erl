-module( sl_sequel_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "common_test/include/ct.hrl" ).
-compile( export_all ).

groups()    -> [{ load_create_select_drop, [sequence], [load, create, insert, select, drop] }].
all()       -> [{ group, load_create_select_drop }].

init_per_suite( Config ) ->
    application:ensure_all_started( sequel ),
    { ok, _Pool } = sequel:connect( ct:get_config( connect_opts ) ),
    Config.

load( Config ) ->
    Dir = ?config( data_dir, Config ),
    { ok, [create, drop, insert, select] } = sl_query_map:load_dir( Dir ).

create( _Config ) ->
    ok = sequel:query( create ).

insert( _Config ) ->
    ok = sequel:execute( insert, ["Hello"] ),
    ok = sequel:execute( insert, ["World"] ).

select( _Config ) ->
    { ok, [#{ id := 1, data := "Hello" }] } = sequel:execute( select, [1] ),
    { ok, [#{ id := 2, data := "World" }] } = sequel:execute( select, [1] ).

drop( _Config ) ->
    ok = sequel:query( drop ).

end_per_suite( _Config ) ->
    application:stop( sequel ).