-module( sequel_sup ).
-behaviour( supervisor ).
-export( [start_link/0, stop/0] ).
-export( [init/1] ).

-define( SERVER, ?MODULE ).

start_link() ->
    supervisor:start_link( { local, ?SERVER }, ?MODULE, [] ).

init( [] ) ->
    { ok, { { one_for_all, 1, 1 }, [
        #{  id      => sl_query_map,
            start   => { sl_query_map, start_link, [] },
            modules => [sl_query_map] },
        #{  id      => sl_statement_map,
            start   => { sl_statement_map, start_link, [] },
            modules => [sl_statement_map] },
        #{  id      => sl_worker_pool,
            start   => { sl_worker_pool, start_link, [] },
            type    => supervisor,
            module  => [sl_worker_pool] }
    ] } }.

stop() ->
    exit( whereis( ?SERVER ), normal ).