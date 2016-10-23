-module( sl_query_map_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "common_test/include/ct.hrl" ).
-compile( export_all ).

all() -> [load_dir_test, load_file_test].

init_per_testcase( _, Config ) ->
	sl_query_map:start_link(),
	Config.

load_dir_test( Config ) ->
	Dir = ?config( data_dir, Config ),
	{ ok, [groups, users] } = sl_query_map:load_dir( Dir ),
	"SELECT * FROM groups" = sl_query_map:get( groups ),
	"SELECT * FROM users" = sl_query_map:get( users ).

load_file_test( Config ) ->
	Dir = ?config( data_dir, Config ),
	{ ok, groups } = sl_query_map:load_file( filename:join( Dir, "groups.sql" ) ),
	"SELECT * FROM groups" = sl_query_map:get( groups ).	

end_per_testcase( _, Config ) ->
	sl_query_map:stop(),
	Config.