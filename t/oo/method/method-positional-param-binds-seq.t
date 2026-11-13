use Test;

# A Seq passed to a method parameter typed `Positional` -- with or without a
# `:D` smiley -- binds through PositionalBindFailover: it is cached into a
# List, as for a sub parameter. Pod::To::HTML's `node2html(Positional:D $node)`
# is called with a Seq.

plan 6;

class C {
    method a(Positional $x)   { $x.^name }
    method b(Positional:D $x) { $x.^name }
    method c(Positional:D $x) { $x }
    multi method m(Positional:D $x) { 'multi ' ~ $x.^name }
    multi method m($x) { 'generic' }
    proto method p(|) {*}
    multi method p(Positional:D $x) { 'proto-multi ' ~ $x.^name }
    multi method p($x) { 'generic' }
}

is C.a((1, 2).map(* + 1)), 'List', 'Positional method parameter caches a Seq';
is C.b((1, 2).map(* + 1)), 'List', 'Positional:D method parameter caches a Seq';
is C.m((1, 2).map(* + 1)), 'multi List', 'Positional:D multi candidate accepts a Seq';
is C.p((1, 2).map(* + 1)), 'proto-multi List', 'Positional:D candidate under an explicit proto';
is-deeply C.c((1, 2).map(* + 1)), (2, 3), 'the cached List holds the values';
is (sub (Positional:D $x) { $x.^name })((1, 2).map(* + 1)), 'List', 'sub parameter agrees';
