# A plain sub's EVAL no longer sees its calling method's `self`, `$?CLASS` and `$?ROLE`

A named sub called from a method used to answer `self`, `$?CLASS` and `$?ROLE`
(through `EVAL`) from the calling method's frame record, because the record
stack answered "the innermost record" whatever frame asked. They are lexical to
a declaration, so a record-less plain-sub frame now answers none of its
caller's. A sub declared in a class or role body keeps the class or role it was
declared in as `$?CLASS` / `$?ROLE`, from any caller, and an `EVAL` that finds
no `$?CLASS` / `$?ROLE` at all raises `X::Undeclared` as rakudo does instead of
yielding `Nil`. ADR-12529 census remainder of #16099.
