-module( sl_statement_map_tests ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "eunit/include/eunit.hrl" ).

put_test() ->
	{ ok, _Map } = sl_statement_map:start_link(),
	sl_statement_map:put( pool_1, test_query, "SELECT * FROM users" ),
	?assertEqual( "SELECT * FROM users", sl_statement_map:get( pool_1, test_query ) ),
	?assertEqual( undefined, sl_statement_map:get( pool_1, undefined_query ) ),
    ?assertEqual( undefined, sl_statement_map:get( pool_2, test_query ) ),
	sl_statement_map:stop().
