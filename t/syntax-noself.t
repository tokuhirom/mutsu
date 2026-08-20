use Test;

plan 11;

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

# A bare `die`/`fail` (no argument) parses to a reference to `$!` (the error
# variable), not the `$!attr` private-twigil form -- so a plain `sub` (which
# has no `self`) nested in a class body may still use a bare `die`/`fail`
# without tripping the attribute-twigil NoSelf check. Regression test for the
# P5tie `array.rakutest` parse bug (see
# news/2026-08/p5tie-array-rakutest-noself-parse-bug.md):
# `class Foo { sub STORESIZE($self,\val) { die } }` used to misparse as
# X::Syntax::NoSelf even though `die` here has nothing to do with `self`.
lives-ok {
    EVAL 'class A6 { sub helper() { die } }; 1'
}, 'bare `die` in a plain sub nested in a class body is not X::Syntax::NoSelf';

lives-ok {
    EVAL 'class A7 { sub helper($x) { fail } }; 1'
}, 'bare `fail` in a plain sub nested in a class body is not X::Syntax::NoSelf';

# A genuine `$!attr` reference in a plain sub nested in a class body is still
# correctly rejected -- the fix must not blanket-disable the check.
throws-like
    'class A8 { has $!x; sub helper() { $!x } }',
    X::Syntax::NoSelf;
