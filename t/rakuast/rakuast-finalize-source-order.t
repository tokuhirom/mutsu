use v6;
use Test;

# ADR-15760 slice 2b: a compilation unit's own names are declared in source
# order when it is finalized. A routine body written before a declaration sees
# the setting's meaning of the name, as in rakudo (measured on 2026.09); only
# routines are hoisted.

plan 8;

sub run-unit(Str $code) {
    my $out = '';
    my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush {} }.new;
    EVAL $code;
    $out
}

is run-unit('say tau; sub f { tau }; constant tau = 1; say f; say tau'),
    "6.283185307179586\n6.283185307179586\n1\n",
    'a routine body before a later constant sees the setting term';

is run-unit('class A { method m { pi } }; my \pi = 2; say A.m; say pi'),
    "3.141592653589793\n2\n",
    'a method body before a later sigilless term sees the setting term';

is run-unit('{ say e }; constant e = 3; say e'),
    "2.718281828459045\n3\n",
    'a block before a later constant sees the setting term';

is run-unit('sub f { X }; constant X = 3; say f; say X'),
    "(X)\n3\n",
    'a routine body before a later capitalised constant sees the setting name';

is run-unit('foo; sub foo { say "hi" }'), "hi\n",
    'a routine is hoisted: it is called before its declaration';

is run-unit('sub f { g }; sub g { 7 }; say f'), "7\n",
    'a routine word inside a body before the routine is late-bound';

is run-unit('constant e = 3; sub f { e }; say f'), "3\n",
    'a declaration before the body is what the body sees';

throws-like { EVAL 'sub g { x }; my \x = 1; say g' }, X::Undeclared::Symbols,
    'a body before a later sigilless term with no setting meaning is undeclared';
