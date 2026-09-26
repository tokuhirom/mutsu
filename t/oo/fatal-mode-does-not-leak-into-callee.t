use Test;

# `use fatal` is a LEXICAL pragma in real Raku: it only affects code
# lexically inside its own scope. A routine declared OUTSIDE a `use fatal`
# block must not run under it merely because it happens to be called from
# inside one -- mutsu used to keep the pragma in a single interpreter-wide
# `fatal_mode` flag, which stayed set for the whole dynamic extent of a call
# (#9521).

plan 4;

sub g($x) { "g-ran" }
sub f($s) { g($s.Int) }

{
    use fatal;
    is (try { f("abc") }) // "died", "g-ran",
        'a Failure produced inside a callee declared outside `use fatal` does'
        ~ ' not explode a nested call, even though the caller is inside `use fatal`';
}

class C {
    method m($x) { "m-ran" }
    method mm($x) { "mm-ran" }
}
sub h($s) { C.m($s.Int) }
sub k($s) { my $c = C; $c.=mm($s.Int); $c }

{
    use fatal;
    is (try { h("abc") }) // "died", "m-ran",
        'a method call inside a callee declared outside `use fatal` does not explode';
    is (try { k("abc") }) // "died", "mm-ran",
        'a mutating method call (`.=`) inside a callee declared outside `use fatal` does not explode';
}

# Sanity: `use fatal` still throws for a callee genuinely declared under it.
sub fails-under-fatal($s) {
    use fatal;
    g($s.Int);
}
dies-ok { fails-under-fatal("abc") },
    'use fatal still explodes inside a routine actually declared under it';
