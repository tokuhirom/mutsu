use Test;

plan 8;

# A `$.attr` accessor used where no `self` is available is X::Syntax::NoSelf.
# This is distinct from bare `self` (X::Syntax::Self::WithoutObject).

throws-like '$.a', X::Syntax::NoSelf, variable => '$.a';

throws-like 'my class B0Rk { $.a }', X::Syntax::NoSelf, variable => '$.a';

# Bare `self` still reports X::Syntax::Self::WithoutObject, not NoSelf.
throws-like 'self', X::Syntax::Self::WithoutObject;

# Inside a method, `$.attr` resolves against the invocant — including in
# nested blocks, loops, gather, and `.map` callbacks lexically within it.
lives-ok {
    EVAL 'class A1 { has $.x = 5; method m { $.x } }; A1.new.m'
}, '$.attr works directly in a method body';

is-deeply
    EVAL('class A2 { has $.x = 5; method m { my $f = { $.x }; $f() } }; A2.new.m'),
    5,
    '$.attr works in a nested closure within a method';

is-deeply
    EVAL('class A3 { has $.n = 3; method m { (1..2).map({ $_ * $.n }).list } }; A3.new.m'),
    (3, 6),
    '$.attr works in a .map callback within a method';

is-deeply
    EVAL('class A4 { has @.xs = 1, 2; method m { gather for @.xs { take $_ } }.list }; A4.new.m'),
    (1, 2),
    '$.attr works in gather/take within a method';

# A submethod (e.g. TWEAK) also provides `self`.
lives-ok {
    EVAL 'class A5 { has $.x = 9; submethod TWEAK { $.x } }; A5.new'
}, '$.attr works in a submethod body';
