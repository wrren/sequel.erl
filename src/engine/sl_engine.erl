%%
%%  @author Warren Kenny
%%  @doc Behaviour for database engines. All database engines should provide functions
%%  for connecting, describing the schema of the current database and querying. 
%%
-module( sl_engine ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-type connection() 	:: pid().
-type query() 		:: atom() | string().
-export_type( [connection/0, query/0] ).

%%
%%  @doc Connect to the database using the provided options. Option formatting depends on the
%%  database type selected.
%%
-callback connect( Options :: map() ) -> { ok, Connection :: connection() } | { error, term() }.

%%
%%  @doc Query the database.
%%
-callback query( Query :: query(), Parameters :: [term()] ) -> { ok, Result :: any() } | { error, term() }.