# `augment class` rejects a `proto method` that a core type already declares

`augment class Str { proto method uc(|) {*} }` was accepted, while Rakudo dies with
`Package 'Str' already has a method 'uc'`. The #10234 conflict check only ran for
`MethodDecl` statements in an augment body; a `proto method` is a separate `ProtoDecl`
statement and skipped it. The augment body now runs the same check for proto methods.
