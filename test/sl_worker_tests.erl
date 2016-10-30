-module( sl_worker_tests ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-include_lib( "eunit/include/eunit.hrl" ).

prepare_test() ->
	{ ok, Pid } = sl_worker:start_link( [default_pool, sl_test_engine, #{}] ),
    ?assertMatch( ok, sl_worker:prepare( Pid, update, "UPDATE foo SET data = ? WHERE id = ?" ) ),
    ?assertMatch( ok, sl_worker:execute( Pid, update, [] ) ),
    ?assertMatch( ok, sl_worker:prepare( Pid, select, "SELECT * FROM foo" ) ),
    ?assertMatch( { ok, _Map }, sl_worker:execute( Pid, select, [] ) ).
