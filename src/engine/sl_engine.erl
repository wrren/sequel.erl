%%
%%  @author Warren Kenny
%%  @doc Behaviour for database engines. All database engines should provide functions
%%  for connecting, describing the schema of the current database and querying. 
%%
-module( sl_engine ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-type connection() 	    :: term().
-type statement()       :: atom().
-type sql() 		    :: string().
-type result()          :: any().

-export_type( [connection/0, statement/0, sql/0, result/0] ).

%%
%%  @doc Connect to the database using the provided options. Option formatting depends on the
%%  database type selected.
%%
-callback connect( PoolId :: atom(), Options :: map() ) -> { ok, Connection :: connection() } | { error, term() }.

%%
%%  @doc Prepare a prepared statement
%%
-callback prepare( Connection :: connection(), Statement :: statement(), SQL :: sql() ) -> ok | { error, term() }.

%%
%%  @doc Execute a raw query
%%
-callback query( Connection :: connection(), SQL :: sql() ) -> { ok, Result :: result() } | { error, term() }.

%%
%%  @doc Execute a prepared statement
%%
-callback execute( Connection :: connection(), Statement :: statement(), Parameters :: [term()] ) -> { ok, Result :: result() } | { error, term() }.