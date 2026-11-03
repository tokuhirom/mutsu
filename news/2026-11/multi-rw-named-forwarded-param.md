# A forwarded read-only named parameter no longer binds an `is rw` multi candidate

`sub f(Str :$x!) { K.c(:$x) }` passes the parameter's read-only cell. A multi
method with both `:$x! is rw` and `:$x! is copy` candidates picked the `is rw` one
and died with "Cannot assign to a readonly variable"; it now falls through to
`is copy`, as rakudo does. PDF::COS `coerce` hit this on every `PDF` test file
that builds a dictionary.
