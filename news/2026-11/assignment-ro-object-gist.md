# `X::Assignment::RO` names a plain object by its `.gist`

`my %h is Bar = ...` and `Bar.new = 3` said `Cannot modify an immutable Bar (Bar<23>)` and an
internal `cannot assign through .new on non-instance`; rakudo says
`Cannot modify an immutable Bar (Bar.new)` for both (#16779). The two sites that hold the interpreter
now word the error with the object's own `.gist`, and an instance's `typename` is its class.
