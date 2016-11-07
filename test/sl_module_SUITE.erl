-module( sl_module_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "common_test/include/ct.hrl" ).
-compile( export_all ).

groups()    -> [{ load_compile_create_select_drop, [sequence], [load, compile, create, insert, select, drop] }].
all()       -> [{ group, load_compile_create_select_drop }].

init_per_suite( Config ) ->
    application:ensure_all_started( sequel ),
    sequel:connect( #{ engine => sqlite, pool_size => 1, path => ":memory:" } ),
    Config.

load( Config ) ->
    Dir = ?config( data_dir, Config ),
    { ok, [create, drop, insert, select] } = sl_query_map:load_dir( Dir ).

compile( _Config ) ->
    Module = lists:foldl(   fun( Query, Module ) ->
                                { ok, Mod2 } = sl_module:add_query( Module, Query, sl_query_map:get( Query ) ),
                                Mod2
                            end, sl_module:new( sql_test ), [create, drop, insert, select] ),
    { ok, _ } = sl_module:compile( Module ).

create( _Config ) ->
    { ok, _ } = sql_test:create( [] ).

insert( _Config ) ->
    sql_test:insert( [<<"Hello">>] ),
    sql_test:insert( [<<"World">>] ).

select( _Config ) ->
    { ok, [#{ id := 1, data := <<"Hello">> }] } = sql_test:select( [1] ),
    { ok, [#{ id := 2, data := <<"World">> }] } = sql_test:select( [2] ).

drop( _Config ) ->
    { ok, _ } = sql_test:drop( [] ).

end_per_suite( _Config ) ->
    application:stop( sequel ).