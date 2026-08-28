use Test;
use MONKEY-SEE-NO-EVAL;

# An EVAL that fails must throw the same way no matter where the EVAL sits.
# Two independent mechanisms used to suppress the throw whenever the EVAL ran
# inside a routine (which is exactly the shape the real `Test.rakumod`'s
# Raku-level `throws-like` puts it in):
#
#  1. `$_` is stored under the sigil-less env key `_`, seeded with the `Any`
#     type object on routine entry, so every "resolve a bare type name through
#     its env alias" lookup read the topic as a `my class _` aliasing `Any`.
#     `10_.0` parses as a speculative `infix:<_>`; inside a routine its
#     unresolvable fallback then became a coercion to `Any` returning
#     `(10, 0.0)` instead of throwing X::Syntax::Confused.
#  2. EVAL's `&?ROUTINE` check was gated on the *caller's* routine stack rather
#     than the snippet's own lexical structure, so `EVAL '&?ROUTINE'` was
#     accepted whenever the EVAL happened to be called from inside a sub.

plan 14;

sub runit(&c) {
    my $ex;
    {
        &c();
        CATCH { default { $ex = $_ } }
    }
    $ex;
}

# A/B: both spellings are X::Syntax::Confused in rakudo; B is the one that used
# to vanish. Run through `runit` so the EVAL is invoked from inside a routine.
isa-ok runit({ EVAL '10_' }), X::Syntax::Confused,
    'EVAL q|10_| throws through a routine-invoked block';
isa-ok runit({ EVAL '10_.0' }), X::Syntax::Confused,
    'EVAL q|10_.0| throws through a routine-invoked block';
# C: &?ROUTINE has no enclosing routine in the EVAL'd compilation unit.
isa-ok runit({ EVAL 'my $baz = try { &?ROUTINE.name };' }), X::Undeclared::Symbols,
    'EVAL q|&?ROUTINE| throws through a routine-invoked block';
# D: the control -- a plain die always propagated.
isa-ok runit({ die 'plain' }), X::AdHoc,
    'a plain die still throws through a routine-invoked block';

# The same failure at mainline, where it always worked.
my $mainline; try { EVAL '10_.0'; CATCH { default { $mainline = $_ } } };
isa-ok $mainline, X::Syntax::Confused, 'B also throws at mainline';

# The `throws-like` shape the real Test module uses.
throws-like { EVAL '10_.0' }, X::Syntax::Confused,
    'throws-like sees the EVAL failure';

# The underlying mechanism, without EVAL: an undeclared word infix / an unknown
# routine named `_` must not resolve to the topic's `Any` inside a routine.
dies-ok { EVAL 'sub uid { 1 _ 2 }; uid()' },
    'an undeclared `_` word infix dies inside a routine';
dies-ok { EVAL 'sub g() { _(1, 2) }; g()' },
    'calling an undeclared routine `_` dies inside a routine';
dies-ok { EVAL 'sub h { my _ $x = 3; $x }; h()' },
    '`_` is not accepted as a type name inside a routine';

# `&?ROUTINE` resolution is lexical in the EVAL'd unit, not caller-dependent.
sub eval-from-a-routine { EVAL 'sub g { &?ROUTINE.name }; g()' }
is eval-from-a-routine(), 'g',
    'a routine the EVAL declares does supply &?ROUTINE (called from a routine)';
is EVAL('sub g2 { &?ROUTINE.name }; g2()'), 'g2',
    'a routine the EVAL declares does supply &?ROUTINE (called from mainline)';
is EVAL('sub g3 { my $b = { &?ROUTINE.name }; $b() }; g3()'), 'g3',
    'a bare block inside the EVAL-declared routine still sees &?ROUTINE';
is EVAL('class C { method m { &?ROUTINE.name } }; C.m'), 'm',
    'a method the EVAL declares supplies &?ROUTINE';
throws-like { EVAL 'sub g4 { }; &?ROUTINE' }, X::Undeclared::Symbols,
    'a mainline &?ROUTINE is undeclared even when the snippet declares a routine';
