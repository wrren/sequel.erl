-module( sl_sequel_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "common_test/include/ct.hrl" ).
-compile( export_all ).

groups()    -> [{ load_create_select_drop, [sequence], [load, create, insert, select, drop] }].
all()       -> [{ group, load_create_select_drop }].

init_per_suite( Config ) ->
    application:ensure_all_started( sequel ),
    sequel:connect( #{ engine => sqlite, pool_size => 1, path => ":memory:" } ),
    Config.

load( Config ) ->
    Dir = ?config( data_dir, Config ),
    { ok, [create, drop, insert, select, { model, create }, { model, drop }, { model, insert }, { model, select }] } = sl_query_map:load_dir( Dir ),
    { [model], [] } = sl_query_map:generate_modules().

create( _Config ) ->
    { ok, _ } = model:create(),
    { ok, _ } = sequel:query( create ).

insert( _Config ) ->
    sequel:prepare( insert, sl_query_map:get( insert ) ),
    { ok, _ } = model:insert( [<<"Test Data">>] ),
    { ok, _ } = sequel:execute( insert, [<<"Hello">>] ),
    { ok, _ } = sequel:execute( insert, [<<"World">>] ).

select( _Config ) ->
    { ok, [#{ id := 1, data := <<"Test Data">> }] } = model:select( [1] ),
    { ok, [#{ id := 1, data := <<"Hello">> }] } = sequel:execute( select, [1] ),
    { ok, [#{ id := 2, data := <<"World">> }] } = sequel:execute( select, [2] ).

drop( _Config ) ->
    { ok, _ } = model:drop(),
    { ok, _ } = sequel:query( drop ).

end_per_suite( _Config ) ->
    application:stop( sequel ).