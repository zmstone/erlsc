
{ roots %% Erlang type spec roots to be extracted from .beam files
, [ {erlsc_sample, person, 0} %% test/erlsc_sample.erl
  ]
}.

{ avsc %% avro schema
, [ {namespace, "com.example"} %% optional
  , {output_dir, "/tmp/erlsc/avsc"} %% the directory where the .avsc files are written
  ]
}.

%% where compiled types are saved to/loaded from
{erlsc_file, "/tmp/erlsc/sample.eterm"}.

