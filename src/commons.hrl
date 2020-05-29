-record(simple_table_definition, {pid_lock :: true | false,
                                  to_disk :: to_disk(),
                                  data_type :: set | bag | duplicate_bag,
                                  keypos :: integer()
                                 }).
-type table_definition() :: #simple_table_definition{}.
-type table_spec() :: {table_name(), table_type(), table_definition()}.
-type table_name() :: string().
-type table_type() :: simple.
-type to_disk() :: only_backups | each_write | integer() | no_persistence.
-type data_type() :: set | bag | duplicate_bag.
-type table_operations() :: open | close | create | read | upsert | update | delete | backup.
-define(DB, "priv/").
