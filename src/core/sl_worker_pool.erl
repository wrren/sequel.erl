-module( sl_worker_pool ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-behaviour( supervisor ).
%% supervisor callbacks
-export( [start_link/0, init/1, add_pool/3] ).

start_link() ->
    supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

init( _ ) ->
    Pools = application:get_env( sequel, pools, [] ),
    PoolSpec = lists:map(   fun ( { PoolName, SizeArgs, WorkerArgs } ) ->
                                PoolArgs = [    { name, { local, PoolName } },
                                                { worker_module, sl_worker } ] ++ SizeArgs,
                                poolboy:child_spec( PoolName, PoolArgs, WorkerArgs )
                         end, Pools ),
    { ok, { { one_for_one, 10, 10 }, PoolSpec } }.

add_pool( Name, PoolArgs, WorkerArgs ) ->
    ChildSpec = poolboy:child_spec( Name, PoolArgs, WorkerArgs ),
    supervisor:start_child( ?MODULE, ChildSpec ).