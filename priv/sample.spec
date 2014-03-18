
{ roots %% Erlang type spec roots to be extracted from .beam files
, [ {erlsc_sample, person, 0}
  ]
}.

{erlsc_file, "/tmp/erlsc/sample.eterm"}. %% where compiled types are saved to / loaded from

{ avsc %% avro schema
, [ {namespace, "com.example"}
  , {output_dir, "/tmp/erlsc/avsc"} %% the directory where the .avsc files are written
  ]
}.

