# A method's typed `$_ is copy` parameter checks assignments

`class W { method m(Int $_ is copy) { $_ = "str" } }` let the body assign a
`Str` to its `Int` topic. A sub's typed `$_ is copy` already refused such an
assignment, and so did a method parameter with any other name. The fast
method-call path registered the parameter's type constraint before it pushed
the method's routine frame. Pushing that frame drops any `$_` constraint
inherited from the caller, and it dropped the method's own constraint with
it. A typed `$_ is copy` is now registered after the frame is pushed, so the
assignment throws `X::TypeCheck::Assignment` as in rakudo (#19150).
