-module( sl_module ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [new/1, add_query/3, compile/1] ).

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
-spec add_query( sequel_module(), atom(), string() ) -> sequel_module().
add_query( Module = #sequel_module{ queries = Queries }, ID, SQL ) ->
    Module#sequel_module{ queries = [{ ID, SQL } | Queries] }.

%%
%%  @doc Compile the given sequel dynamic module
%%
-spec compile( sequel_module() ) -> { ok, module() } | { error, term() }.
compile( #sequel_module{ name = Name, queries = Queries } ) ->
    QueriesUnique       = lists:usort( fun( { ID, _ }, { ID, _ } ) -> true; ( _, _ ) -> false end, Queries ),
    Module              = erl_syntax:attribute( erl_syntax:atom( module ), [erl_syntax:atom( Name )] ),
    ModForm             = erl_syntax:revert( Module ),
    FunctionExports     = query_function_exports( QueriesUnique ),
    Functions           = lists:foldl(  fun( Functions, Out ) -> 
                                            lists:append( Functions, Out )
                                        end, [], [query_functions( Name, ID, SQL ) || { ID, SQL } <- QueriesUnique] ),
    Forms               = [ModForm, FunctionExports | Functions],
    case compile:forms( Forms, [verbose, report_errors, report_warnings] ) of
        { ok, Mod, Bin } ->
            load( Mod, Bin );
        { error, Errors, Warnings } ->
            { error, { { errors, Errors }, { warnings, Warnings } } };
        error ->
            { error, { compile_failure, Forms } }
    end.

%%
%%  @doc Attempt to load the provided BEAM binary
%%
load( Module, Binary ) ->
    case code:load_binary( Module, [], Binary ) of
        { module, Module } ->
            { ok, Module };
        { error, Reason } ->
            { error, Reason }
    end.

%%
%%  @doc Given a set of query names, generate a function export expression for a set of functions with matching names and an arity of 1
%%   
query_function_exports( Queries ) ->
    FunctionNames       = lists:usort( [Name || { Name, _SQL } <- Queries] ),
    FunctionExports1    = [erl_syntax:arity_qualifier( erl_syntax:atom( Name ), erl_syntax:integer( 0 ) ) || Name <- FunctionNames],
    FunctionExports2    = [erl_syntax:arity_qualifier( erl_syntax:atom( Name ), erl_syntax:integer( 1 ) ) || Name <- FunctionNames],
    FunctionExports3    = [erl_syntax:arity_qualifier( erl_syntax:atom( Name ), erl_syntax:integer( 2 ) ) || Name <- FunctionNames],
    Export              = erl_syntax:attribute( erl_syntax:atom( export ), [erl_syntax:list( FunctionExports1 ++ FunctionExports2 ++ FunctionExports3 )] ),
    erl_syntax:revert( Export ).

%%
%%  @doc Generate a function AST for a function with the given name that executes the provided SQL as a prepared statement. Two function 
%%  clauses are generated; one that takes a single variable which is the list of arguments to be bound to the prepared statement before
%%  execution on the default connection pool and the second take an additional initial parameter indicating the pool on which the statement 
%%  should be executed.
%%
query_functions( Module, Name, SQL ) ->
    ArgsVar     = erl_syntax:variable( "Args" ),
    PoolVar     = erl_syntax:variable( "PoolId" ),
    Statement   = erl_syntax:tuple( [erl_syntax:atom( Module ), erl_syntax:atom( Name )] ),
    
    Body1        = [
        erl_syntax:application( erl_syntax:atom( sequel ), erl_syntax:atom( prepare ), [Statement, erl_syntax:string( SQL )] ),
        erl_syntax:application( erl_syntax:atom( sequel ), erl_syntax:atom( execute ), [Statement, erl_syntax:list( [] )] )
    ],
    Clause1      = erl_syntax:clause( [], [], Body1 ),
    
    Body2        = [
        erl_syntax:application( erl_syntax:atom( sequel ), erl_syntax:atom( prepare ), [Statement, erl_syntax:string( SQL )] ),
        erl_syntax:application( erl_syntax:atom( sequel ), erl_syntax:atom( execute ), [Statement, ArgsVar] )
    ],
    Clause2      = erl_syntax:clause( [ArgsVar], [], Body2 ),
    
    Body3        = [
        erl_syntax:application( erl_syntax:atom( sequel ), erl_syntax:atom( prepare ), [Statement, erl_syntax:string( SQL )] ),
        erl_syntax:application( erl_syntax:atom( sequel ), erl_syntax:atom( execute ), [PoolVar, Statement, ArgsVar] )
    ],
    Clause3      = erl_syntax:clause( [PoolVar, ArgsVar], [], Body3 ),

    Fun1 = erl_syntax:revert( erl_syntax:function( erl_syntax:atom( Name ), [Clause1] ) ),
    Fun2 = erl_syntax:revert( erl_syntax:function( erl_syntax:atom( Name ), [Clause2] ) ),
    Fun3 = erl_syntax:revert( erl_syntax:function( erl_syntax:atom( Name ), [Clause3] ) ),

    [Fun1, Fun2, Fun3].
