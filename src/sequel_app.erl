-module( sequel_app ).
-behaviour( application ).
-export( [start/2, stop/1] ).

-include_lib( "kernel/include/file.hrl" ).

%%
%%  @doc If lager is available, outputs an info-level message indicating that the specified modules 
%%  were generated and failed to generate.
%%
log_generate_result( Loaded, Failed ) ->
    case code:is_loaded( lager ) of
        false -> 
            ok;
        _ ->
            lager:info( "Sequel Module Generator Loaded: ~p, Failed: ~p", [Loaded, Failed] )
    end.

start( _StartType, _StartArgs ) ->
    Start = sequel_sup:start_link(),
    SQLDir = filename:join( code:priv_dir( sequel ), application:get_env( sequel, dir, "sql" ) ),
    case file:read_file_info( SQLDir ) of
        { ok, #file_info{ type = directory } } ->
            sl_query_map:load_dir( SQLDir ),
            { Loaded, Failed } = sl_query_map:generate_modules(),
            log_generate_result( Loaded, Failed );
        _ -> 
            ok
    end,
    Start.

stop( _State ) ->
    ok.
