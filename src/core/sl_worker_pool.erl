-module( sl_worker_pool ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-behaviour( supervisor ).
%% supervisor callbacks
-export( [start_link/0, init/1, add_pool/3] ).

start_link() ->
    supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

init( _ ) ->
    Pools = application:get_env( sequel, pools, [] ),
    PoolSpec = lists:map(   fun ( { PoolId, SizeArgs, WorkerArgs } ) ->
                                PoolArgs = [    { name, { local, PoolId } },
                                                { worker_module, sl_worker } ] ++ SizeArgs,
                                { size, PoolSize } = lists:keyfind( size, 1, SizeArgs ),
                                poolboy:child_spec( PoolId, PoolArgs, [{ PoolId, PoolSize } | WorkerArgs] )
                         end, Pools ),
    { ok, { { one_for_one, 10, 10 }, PoolSpec } }.

add_pool( PoolId, PoolArgs, WorkerArgs ) ->
    { size, PoolSize } = lists:keyfind( size, 1, PoolArgs ),
    ChildSpec = poolboy:child_spec( PoolId, PoolArgs, [{ PoolId, PoolSize } | WorkerArgs] ),
    supervisor:start_child( ?MODULE, ChildSpec ).