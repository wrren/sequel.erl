-module( sl_sequel_tests ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "eunit/include/eunit.hrl" ).

add_pool_test() ->
	application:ensure_all_started( sequel ),
    sequel:connect( #{ engine => test } ),
    ?assertMatch( ok, sequel:prepare( update, "UPDATE foo SET data = ? WHERE id = ?" ) ),
    ?assertMatch( ok, sequel:execute( update, [] ) ),
    ?assertMatch( ok, sequel:prepare( select, "SELECT * FROM foo" ) ),
    ?assertMatch( { ok, [#{ id := 1 }, #{ id := 2 }] }, sequel:execute( select, [] ) ),
    ?assertMatch( { ok, [#{ id := 3 }, #{ id := 4 }] }, sequel:execute( select, [3, 4] ) ),
    sequel_sup:stop().
