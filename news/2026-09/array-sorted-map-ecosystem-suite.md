# Array::Sorted::Map passes its ecosystem suite

`Array::Sorted::Map` 0.0.4 and its `Array::Sorted::Util` 0.0.11 dependency
now pass all six test files under mutsu. The compatibility work fills in the
native NQP integer division and comparison operations used by the binary
search, preserves native integer payloads when boxing `Int` subclasses, and
keeps containerized array values and associated slurpy storage visible to the
module's existing code.
