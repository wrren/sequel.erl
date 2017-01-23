-module( sl_mysql_engine ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-export( [connect/2, execute/3, prepare/3, query/2] ).
-behaviour( sl_engine ).
-include_lib( "mysql/include/records.hrl" ).

connect( { _PoolID, _Size }, Args = #{  database := Database, 
                                        username := Username,
                                        password := Password }) ->
    case mysql:start_link([ { host, maps:get( host, Args, "localhost" ) },
                            { user, Username }, 
                            { password, Password },
			                { database, Database }] )
	of
		{ ok, Conn } -> 
			{ ok, Conn };
		{ error, Reason } -> 
			{ error, Reason }
    end.

prepare( Conn, ID, SQL ) ->
    case mysql:prepare( Conn, ID, SQL ) of
		{ ok, ID } -> 
			{ ok, Conn };
		{ error, Reason } -> 
			{ error, Reason }
    end.

query( Conn, SQL ) ->
    case mysql:query( Conn, SQL ) of
		ok ->
			{ ok, no_results };
		{ ok, ColumnNames, Rows } -> 
			{ ok, [maps:from_list( lists:zip( ColumnNames, Row ) ) || Row <- Rows] };
		{ error, Reason } -> 
			{ error, Reason }
    end.

execute( Conn, ID, Args ) ->
	case mysql:execute( Conn, ID, Args ) of
		ok ->
			{ ok, no_results };
		{ ok, ColumnNames, Rows } -> 
			ColumnNameAtoms = [binary_to_atom( Name, utf8 ) || Name <- ColumnNames],
			{ ok, [maps:from_list( lists:zip( ColumnNameAtoms, Row ) ) || Row <- Rows] };
		{ error, Reason } -> 
			{ error, Reason }
	end.