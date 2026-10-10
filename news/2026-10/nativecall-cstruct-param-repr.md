# A signature parameter's type keeps a `my class` CStruct representation

`Parameter.type` for a parameter typed with a file-scoped `my class ... is repr('CStruct')`
returned a bare `Package` spelling instead of the lexical class, so `.REPR` answered `P6opaque`.
Upstream NativeCall's `check_routine_sanity` then warned "Not an accepted NativeCall type" for
every native sub taking such a struct (Compress::Bzip2::Raw's `bz_stream`, seen from
Net::Ethereum). Signature types now resolve through the scoped type object.
