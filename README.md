
# ERLangSChema

Library *erlsc* provides APIs to:
1. Extracts Erlang type specs into [avro](http://avro.apache.org/) schema,
2. Serialize Eralng terms directly into avro wire format objects.

## Erlang specs --> avro schema
Schema extraction pecs are either passed in to erlsc:compile/1 directly
as a proplist or defined in a eterm file as *priv/sample.spec*

```
erlsc:compile([{roots, [{erlsc_sample, person, 0}]},
               {avsc,  [{namespace, "com.example"},
                        {output_dir, "/tmp/erlsc/avsc"}]}
              ]).
```
or
```
erlsc:compile({file, "priv/sample.spec"}).
```

## Erlang Terms --> avro objects

The return value of erlsc:compile is a data encoder/serializer function:

```
fun((root_id(), Data::term(), Options::list()) ->
      {TypeName::binary(), Data::binary()}).

```

# Example

To compile spec erlsc_sample:name/0 to avro schema:
```
> Encoder = erlsc:compile([{roots, [{erlsc_sample, name, 0}]},
                           {avsc,  [{output_dir, "/tmp/sample-name/avsc"}]}
                          ]).
```
The avro schema file erlsc_sample.name_0.avsc in /tmp/sample-name/avsc/ is as below:
```
{
    "name": "erlsc_sample.name_0",
    "type": "record",
    "fields":
    [
        {
            "name": "FirstName",
            "type": "string"
        },
        {
            "name": "MiddleName",
            "type":
            [
                "null",
                "string"
            ]
        },
        {
            "name": "LastName",
            "type": "string"
        }
    ]
}
```
To encode a name into avro object in JSON format:
```
> Encoder({erlsc_sample, name, 0}, {"Viktor", "H", "Ye"}, [json]).
{<<"com.example.erlsc_sample.name_0">>,
 <<"{\"FirstName\":\"Viktor\",\"MiddleName\":{\"string\":\"H\"},\"LastName\":\"Ye\"}">>}
```

# Limitations

1. no avro 'int' type support, always use 'long' instead
2. no avro 'float' type support, always use 'double' instead
3. no avro Maps type support
4. no avro Fixed type support
5. no default value support
6. record field names in Erlang spec should contain [A-Za-z0-9_] only

