-module( sequel_sup ).
-behaviour( supervisor ).
-export( [start_link/0] ).
-export( [init/1] ).

-define( SERVER, ?MODULE ).

start_link() ->
    supervisor:start_link( { local, ?SERVER }, ?MODULE, [] ).

init( [] ) ->
    { ok, { { one_for_all, 1, 1 }, [
        #{  id      => sl_query_map,
            start   => { sl_query_map, start_link, [] },
            modules => [sl_query_map] }
    ] } }.