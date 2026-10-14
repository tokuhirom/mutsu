# A built-in method with too many positionals fails to bind, not "No such method"

`"x".uc(1)`, `5.sqrt(2)` and `"3".Int(1)` answered `X::Method::NotFound`; Rakudo reports
`Too many positionals passed; expected 1 argument but got 2`. The last fallback of the
method dispatch now asks the method table: when the receiver's shape has rows for the
name, none slurpy, and the call has more positionals than the widest row, it raises the
bind failure with the row arity plus the invocant (#13203).
