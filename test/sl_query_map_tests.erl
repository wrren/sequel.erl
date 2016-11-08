-module( sl_query_map_tests ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "eunit/include/eunit.hrl" ).

put_test() ->
	{ ok, _Map } = sl_query_map:start_link(),
	sl_query_map:put( test_query, "SELECT * FROM users" ),
	?assertEqual( "SELECT * FROM users", sl_query_map:get( test_query ) ),
	?assertEqual( undefined, sl_query_map:get( undefined_query ) ),
	sl_query_map:stop().

generate_modules_test() ->
	application:ensure_all_started( sequel ),
	sl_query_map:put( test_query, "SELECT * FROM users" ),
	sl_query_map:put( { users, create }, "CREATE TABLE users ( id SERIAL PRIMARY KEY, username VARCHAR( 60 ) )" ),
	sl_query_map:put( { users, select }, "SELECT * FROM users" ),
	?assertEqual( { [users], [] }, sl_query_map:generate_modules() ),
	application:stop( sequel ).