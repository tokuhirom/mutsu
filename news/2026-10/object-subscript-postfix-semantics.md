# Object subscripts and imaginary postfix dispatch

Positional indexing of an ordinary object no longer calls its associative
accessor, and unsupported associative indexing returns a Failure. Postfix
`i` rejects objects outside its Numeric/Cool candidates and propagates a
valid candidate's coercion exception.

Hyper subscripts retain their operator identity and consult user
postcircumfix candidates. Explicit hyper AT-POS/AT-KEY method calls keep
their method semantics. RakuAST read/lower round trips preserve the hyper
subscript operator too.
