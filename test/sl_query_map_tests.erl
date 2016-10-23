-module( sl_query_map_tests ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "eunit/include/eunit.hrl" ).

put_test() ->
	{ ok, _Map } = sl_query_map:start_link(),
	sl_query_map:put( test_query, "SELECT * FROM users" ),
	?assertEqual( "SELECT * FROM users", sl_query_map:get( test_query ) ),
	?assertEqual( undefined, sl_query_map:get( undefined_query ) ),
	sl_query_map:stop().
