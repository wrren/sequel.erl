sequel
=====

Erlang library for simplifying interactions with relational databases. Sequel abstracts the underlying
database away and provides a fast template-based approach to referencing queries. Rather than including
SQL directly in your code, you can load a set of files containing SQL and they'll automatically be added
to a query storage process and associated with either their filename (excluding extension) in atom form or 
under a name you specify.

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