%%
%%  @author Warren Kenny
%%  @doc Behaviour for database engines. All database engines should provide functions
%%  for connecting, describing the schema of the current database and querying. 
%%
-module( sl_engine ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-type connection() :: pid().

%%
%%  @doc Connect to the database using the provided options. Option formatting depends on the
%%  database type selected.
%%
-callback connect( Options :: map() ) -> { ok, Connection :: connection() } | { error, term() }.

%%
%%  @doc Read the table schema for the currently connected database.
%%
-callback schema( Connection :: connection() ) -> { ok, [sl_schema:table()] } | { error, term() }.

%%
%%  @doc Query the database.
%%
-callback query( Connection :: connection(), Query :: string(), Parameters :: [term()] ) -> { ok, Result :: any() } | { error, term() }.