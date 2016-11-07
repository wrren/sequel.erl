-module( sl_module ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [new/1, add_query/2, compile/1] ).

-record( sequel_module, {   name    :: atom(),
                            queries :: [{ atom(), string() }]
} ).

-type sequel_module() :: #sequel_module{}.

%%
%%  @doc Create a new sequel dynamic module.
%%
-spec new( atom() ) -> sequel_module().
new( Name ) when is_atom( Name ) ->
    #sequel_module{ name = Name, queries = [] }.

%%
%%  @doc Add the query with the specified ID from the query map to this module. This function will
%%  fail if the given query isn't stored in the query map.
%%
-spec add_query( sequel_module(), atom() ) -> { ok, sequel_module() } | { error, term() }.
add_query( Module = #sequel_module{ queries = Queries }, Query ) ->
    case sl_query_map:get( Query ) of
        undefined ->
            { error, { undefined_query, Query } };
        SQL ->
            { ok, Module#sequel_module{ queries = [{ Query, SQL } | Queries] } }
    end.

%%
%%  @doc Compile the given sequel dynamic module
%%
-spec compile( sequel_module() ) -> { ok, module() } | { error, term() }.
compile( #sequel_module{ name = Name, queries = Queries } ) ->
    QueriesUnique       = lists:usort( fun( { ID, _ }, { ID, _ } ) -> true; ( _, _ ) -> false end, Queries ),
    Module              = erl_syntax:attribute( erl_syntax:atom( module ), [erl_syntax:atom( Name )] ),
    ModForm             = erl_syntax:revert( Module ),
    FunctionExports     = query_function_exports( QueriesUnique ),
    Functions           = [query_function( ID, SQL ) || { ID, SQL } <- QueriesUnique],
    case compile:forms( [ModForm, FunctionExports] ++ Functions, [verbose, report_errors, report_warnings] ) of
        { ok, Mod, Bin } ->
            code:load_binary( Mod, [], Bin ),
            { ok, Mod };
        { error, Errors, Warnings } ->
            { error, { { errors, Errors }, { warnings, Warnings } } };
        error ->
            { error, { compile_failure, [ModForm, FunctionExports, Functions] } }
    end.
        
query_function_exports( Queries ) ->
    FunctionNames   = lists:usort( [Name || { Name, _SQL } <- Queries] ),
    FunctionExports = [erl_syntax:list( [erl_syntax:arity_qualifier( erl_syntax:atom( Name ), erl_syntax:integer( 1 ) ) || Name <- FunctionNames] )],
    Export          = erl_syntax:attribute( erl_syntax:atom( export ), FunctionExports ),
    erl_syntax:revert( Export ).

query_function( Name, SQL ) ->
    ArgsVar     = erl_syntax:variable( "Args" ),
    Body        = [erl_syntax:application( erl_syntax:atom( sequel ), erl_syntax:atom( execute ), [erl_syntax:atom( Name ), ArgsVar] )],
    Clause      = erl_syntax:clause( [ArgsVar], [], Body ),
    erl_syntax:revert( erl_syntax:function( erl_syntax:atom( Name ), [Clause] ) ).
