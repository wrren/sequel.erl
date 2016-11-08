sequel
=====

Erlang library for simplifying interactions with relational databases. Sequel abstracts the underlying
database away and provides a fast template-based approach to referencing queries. Rather than including
SQL directly in your code, you can load a set of files containing SQL and they'll automatically be added
to a query storage process and associated with either their filename (excluding extension) in atom form or 
under a name you specify.

In addition, sequel can dynamically compile your queries into modules and functions. SQL files in nested directories 
can be compiled together into a new module with the same name as the parent directory, allowing models to be 
defined purely using SQL files and the directory structure.

Usage
---

```erlang

%%
%%  Directory Structure (priv/sql):
%%  priv/
%%      sql/
%%          item_count.sql
%%          user/
%%              create_table.sql
%%              create.sql
%%              find_by_email.sql
%%

application:start( sequel ),
sequel:connect( #{ pool_size => 1, engine => sqlite, path => filename:join( code:priv_dir( ?APPLICATION ), "db.sqlite" ) } ),
%% Load all queries under priv/sql...
sl_query_map:load_dir( filename:join( code:priv_dir( ?APPLICATION ), "sql" ) ),

%% Prepare and execute item_count.sql
{ ok, [Count] } = sequel:execute( item_count, [] ),

%% Generate modules based on directory structure (user module)
sl_query_map:generate_modules(),

%% Prepares and executes sql/user/create_table.sql
user:create_table(),

%% Prepares and executes sql/user/create.sql, binding the given arguments to the prepared statement
user:create( ["user@example.com", "Joe", "Bloggs"] ),

%% Prepares and executes sql/user/find_by_email.sql, binding the given arguments to the prepared statement
{ ok, [#{   id := ID, 
            email := "user@example.com", 
            first_name := "Joe", 
            last_name := "Bloggs" }] } = user:find_by_email( ["user@example.com"] ),

%% All queries are available from the root sequel module using tuples, allowing arbitrary levels of nesting
{ ok, [#{   id := ID, 
            email := "user@example.com", 
            first_name := "Joe", 
            last_name := "Bloggs" }] } = sequel:execute( { user, find_by_email }, ["user@example.com"] ).


```

Build
-----

Add ```sequel``` to your rebar3 dependencies:

```erlang

{ deps, [
    { sequel, { git, "git://github.com/wrren/sequel.erl.git", { branch, "master" } } }
] }

```

    $ rebar3 compile

Test
----

    $ rebar3 do compile, eunit, ct

Configure
---------

Connect to your database at runtime using ```sequel:connect/1```, this will start up the default connection
pool and connect the configured number of workers to the specified database. Connection options are in the form:

```erlang
#{  pool_size       := non_neg_integer(),           %% Connection Pool Size
    max_overflow    := non_neg_integer(),           %% Maximum number of workers created if pool is empty
    engine          := mysql | postgres | sqlite,   %% Database Engine    
    ...
}

%% Extend the Options map with engine-specific keys:

%% mysql & postgres
#{  host            := string(),                    %% Database Host
    username        := string(),                    %% DB User Username
    password        := string(),                    %% DB User Password
    database        := string()                     %% Database
}

```
