use Test;

# The undeclared-attribute check walks the whole method body (ADR-0137
# typed visitor), so `$!x` is rejected wherever rakudo rejects it at compile
# time -- not only in the positions an older hand-rolled walker listed.

plan 16;

throws-like 'my class C1 { method m { sub f { $!nope } } }', X::Attribute::Undeclared,
    symbol => '$!nope', 'in a sub nested in a method';
throws-like 'my class C2 { method m { my $c = -> $y = $!nope { } } }', X::Attribute::Undeclared,
    symbol => '$!nope', 'in a pointy block parameter default';
throws-like 'my class C3 { method m { say "{$!nope}" } }', X::Attribute::Undeclared,
    symbol => '$!nope', 'in an interpolated block';
throws-like 'my class C4 { method m { my %h = a => { $!nope } } }', X::Attribute::Undeclared,
    symbol => '$!nope', 'in a closure stored in a hash';
throws-like 'my class C5 { method m { foo(:x($!nope)) } }; sub foo(*%) { }',
    X::Attribute::Undeclared, symbol => '$!nope', 'in a named argument';

# `@!`, `%!` and `&!` attributes are checked too, reads and assignments alike.
# Under `EVAL` the undeclared-variable pre-check leaves every `!`-twigil name to
# this check, so the exception is X::Attribute::Undeclared, never X::Undeclared.
for <@!nope %!nope &!nope> -> $sym {
    throws-like "my class CE \{ has \$.a; method m \{ $sym \} \}", X::Attribute::Undeclared,
        symbol => $sym, "EVAL: an undeclared $sym attribute";
}
throws-like 'my class CA { has $.a; method m { @!nope = 1 } }', X::Attribute::Undeclared,
    symbol => '@!nope', 'EVAL: an assignment to an undeclared @! attribute';

# The same, run as a program (a compile error rather than a thrown exception).
sub compile-error(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    $proc.out.slurp(:close);
    $proc.err.slurp(:close)
}
like compile-error('class C { has $.a; method m { @!nope } }'),
    /'Attribute @!nope not declared in class C'/, 'an undeclared @! attribute';
like compile-error('class C { has $.a; method m { %!nope<x> = 1 } }'),
    /'Attribute %!nope not declared in class C'/, 'an undeclared %! attribute';
like compile-error('class C { has $.a; method m { @!nope = 1, 2 } }'),
    /'Attribute @!nope not declared in class C'/,
    'an assignment to an undeclared @! attribute';
like compile-error('class C { has $.a; method m { &!nope } }'),
    /'Attribute &!nope not declared in class C'/, 'an undeclared &! attribute';

my class Good {
    has @.a;
    has %.h;
    has &.cb;
    method m { @!a = 1, 2; %!h = x => 1; @!a.elems + %!h.elems }
    method call { &!cb() }
}
is Good.new.m, 3, 'declared @! and %! attributes pass';
is Good.new(cb => { 42 }).call, 42, 'a declared &! attribute passes';

# A nested type is validated against its own attributes, not the outer one's.
my class Outer {
    has $.a;
    method m { my class Inner { has $.b; method n { $!b } }; Inner.new(b => 7).n }
}
is Outer.new.m, 7, 'a nested class reads its own attribute';
