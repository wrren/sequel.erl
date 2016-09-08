-module( sl_schema ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-type table_name()      :: binary().
-type column_name()     :: binary().
-type column_type()     :: integer | float | string | boolean.
-type column_option()   :: auto_increment | primary_key | index | unique.

-type column()          :: { column_name(), column_type(), [column_option()] }.
-type table()           :: { table_name(), [column()] }.

-export_type( [table/0, column/0, table_name/0, column_name/0, column_type/0, column_option/0] ).