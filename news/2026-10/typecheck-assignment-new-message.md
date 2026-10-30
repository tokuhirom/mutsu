# A hand-built `X::TypeCheck::Assignment` has rakudo's message

`X::TypeCheck::Assignment.new(:symbol, :got, :expected)` has an empty `.message` in mutsu, so
`throws-like { ... }, X::TypeCheck::Assignment, message => rx/value/` could never match. Red's
`Red::Model.new` rejects a mistyped column that way. The native exception row now renders
`Type check failed in assignment to $!value; expected Str but got Int (42)` from the typed
attributes (the interpreter-raised ones still carry their stored message). Red `t/35-create`
now passes.
