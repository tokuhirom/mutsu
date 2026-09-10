use Test;

# A `&cb:(...)` parameter carrying an explicit code signature is now OTF-compiled
# (dispatched as bytecode) instead of falling back to the tree-walk interpreter.
# The winning candidate is still resolved by signature-matching, so results are
# unchanged — this only removes the interpreter fallback. (PLAN §2-D.)

plan 6;

# Single candidate with a code-signature callback param.
{
    sub one(&cb:(Int)) { cb(10) }
    is one(-> Int $n { $n * 3 }), 30, "single code-signature &cb param";
}

# Multi dispatch selected by the callback's signature.
{
    multi run-cb(&cb:(Int)) { "int:" ~ cb(5) }
    multi run-cb(&cb:(Str)) { "str:" ~ cb("x") }
    is run-cb(-> Int $n { $n * 2 }), "int:10", "multi picks (Int) callback candidate";
    is run-cb(-> Str $s { $s ~ "!" }), "str:x!", "multi picks (Str) callback candidate";
}

# Multi-arg and return-typed callback signatures.
{
    multi g(&c:(Int, Int)) { c(2, 3) }
    multi g(&c:(Str)) { c("a") }
    is g(-> Int $a, Int $b { $a + $b }), 5, "two-Int callback signature";
    is g(-> Str $s { $s x 2 }), "aa", "one-Str callback signature";
}

# A specific code-signature candidate alongside an ordinary param still works.
{
    multi mix-cb(Int $x, &cb:(Int)) { $x + cb($x) }
    is mix-cb(4, -> Int $n { $n * 10 }), 44, "code-signature param mixed with a normal param";
}
