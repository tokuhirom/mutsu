# A unit-scoped sub's EVAL no longer sees its calling method's `self`, `$?CLASS` and `$?ROLE`

A named sub called from a method used to answer `self`, `$?CLASS` and `$?ROLE`
(through `EVAL`) from the calling method's frame record, because the record
stack answered "the innermost record" whatever frame asked. They are lexical to
a declaration, so a sub declared outside every routine body no longer continues
its caller's record. A sub declared in a class or role body keeps the class or
role it was declared in as `$?CLASS` / `$?ROLE`, from any caller; a sub nested
in a method still reads that method's `self`. ADR-12529 census remainder of
#16099. (rakudo reports `$?CLASS` undeclared there; mutsu answers `Nil`, as it
does at file scope.)
