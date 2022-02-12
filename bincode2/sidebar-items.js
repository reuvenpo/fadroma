initSidebarItems({"enum":[["ErrorKind","The kind of error that can be produced during a serialization or deserialization."],["LengthOption","Used to specify the unit used for length of strings and arrays via `config.string_length` or `config.array_length`."]],"fn":[["config","Get a default configuration object."],["deserialize","Deserializes a slice of bytes into an instance of `T` using the default configuration."],["deserialize_from","Deserializes an object directly from a `Read`er using the default configuration."],["deserialize_from_custom","Deserializes an object from a custom `BincodeRead`er using the default configuration. It is highly recommended to use `deserialize_from` unless you need to implement `BincodeRead` for performance reasons."],["serialize","Serializes a serializable object into a `Vec` of bytes using the default configuration."],["serialize_into","Serializes an object directly into a `Writer` using the default configuration."],["serialized_size","Returns the size that an object would be if serialized using Bincode with the default configuration."]],"struct":[["Config","A configuration builder whose options Bincode will use while serializing and deserializing."]],"trait":[["BincodeRead","An optional Read trait for advanced Bincode usage."]],"type":[["Error","An error that can be produced during (de)serializing."],["Result","The result of a serialization or deserialization operation."]]});