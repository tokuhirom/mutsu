# User infix `is rw` parameters accept a Proxy operand

A Proxy returned by an `is rw` method is now treated as a writable lvalue by multi dispatch and by the binder, so `multi infix:<~>(Str() $a is rw, AST $b)` is selected for `$obj.title ~ AST.new` as in Rakudo (#12573).
