# `.clone` of a user object skips the method cascade

On an instance of a user class that declares no `clone`, the compiled method
dispatch found no user method and handed `.clone(...)` to the interpreter's
whole instance cascade, about 65k instructions before its `clone` arm
(`native_instance_clone_value`) ran. EEL's fork copies its frames,
instructions and goals with `.clone(:field(...))` (#17530).

A lane next to the `Attribute` metamethod lane now calls that arm directly,
once the arguments are bound, for an ordinary user class (not a CStruct, no
`IO::Handle`/`IO::Path` base, no built-in payload) called with named twiddles
only, when nothing wraps a method or declares a `^find_method`.

`.clone` of a two-attribute object went from 111k to 46k instructions
(release build, callgrind). Three differences from rakudo that predate the
lane are #17556.
