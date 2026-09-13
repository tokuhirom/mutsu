use v6.c;
use Test;

plan 8;

my $plus = CORE::<&infix:<+>>;
ok $plus.defined, 'CORE stash exposes the infix addition routine';
is CORE::<&infix:<+>>(1, 2), 3,
    'the CORE infix addition routine is callable';
ok CORE::<&prefix:<->>.defined, 'CORE stash exposes the prefix negation routine';
is CORE::<&prefix:<->>(3), -3,
    'the CORE prefix negation routine is callable';
ok CORE::<&say>.defined, 'CORE stash still exposes ordinary routines';

if CORE::<&infix:<+>> {
    pass 'nested-angle CORE stash lookup parses in conditional position';
}
else {
    flunk 'nested-angle CORE stash lookup parses in conditional position';
}

my $assigned = CORE::<&infix:<+>>;
ok $assigned.defined, 'nested-angle CORE stash lookup parses after assignment';

my %hash = '&infix:<+>' => 'present';
is %hash<&infix:<+>>, 'present',
    'nested-angle operator names remain valid in ordinary hash subscripts';
