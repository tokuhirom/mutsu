# A module loaded inside a routine keeps its nested lexical types

A `require` inside a routine runs the module body against the routine's frame, so a method of the module
looked up its `my class` types by their registry storage name once that frame was gone. The lookup that
guards the scan (`Registry::has_lexical_type_key_for`) only knew the part of a key before its first NUL,
so a class nested in another lexical class (`Outer\0<id>::Inner\0<id>`) was never found, and the scan
that follows could return the nested type for the outer one. The guard now knows every NUL-terminated
prefix, and the scan skips types nested in the one asked for (#14143).
