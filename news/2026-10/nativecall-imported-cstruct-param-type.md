# An imported `is repr('CStruct')` lexical class is a NativeCall type

`my class NS::CProb is export is repr('CStruct')` is registered under a
storage name with a declaration-id suffix (`NS::Prob::NS::CProb\0<id>`).
`declared_class_repr`, which answers `.REPR` for a type object spelled by its
source name (a routine signature's parameter type), compared that spelling to
the suffixed name's last segment, so the imported class reported `P6opaque`
and NativeCall's `check_routine_sanity` warned "Not an accepted NativeCall
type" once per native routine of `Algorithm::LibSVM` (#15812). The storage
suffix is now ignored in the comparison.
