use Test;

# Two plain strings compare in place (ADR-18357 stage 2), but only while no
# user candidate of the operator is in scope: a declared, a block-scoped or an
# allomorph/mixin operand still reaches the general path, which asks it.

plan 14;

sub is-space(Str $ch) { $ch eq ' ' || $ch eq "\n" || $ch eq "\t" }
my int $spaces = 0;
for "a b\nc\td".comb -> $c { $spaces++ if is-space($c) }
is $spaces, 3, 'plain compares in a predicate';

{
    multi sub infix:<eq>(Str $a, Str $b where 'magic') { 'user eq' }
    is ('x' eq 'magic'), 'user eq', 'a declared candidate answers its own case';
    is ('x' eq 'y'), False, '... and the core one the rest';
}

{
    sub infix:<lt>(Str $a, Str $b) { 'mine' }
    is ('a' lt 'b'), 'mine', 'a declared `lt` replaces the core one in its scope';
}
is ('a' lt 'b'), True, 'the core `lt` is back outside that scope';

ok 'abc' ne 'abd', 'ne';
nok 'b' le 'a', 'le';
ok 'b' ge 'a', 'ge';
ok 'a' gt '', 'gt against the empty string';

ok <42> eq '42', 'an IntStr allomorph compares as its string';
my $mixed = 'hi' but role { method Str { 'HI' } };
ok $mixed eq 'hi', 'a mixin compares as its Stringy value';
ok ('a' | 'b') eq 'b', 'a Junction still autothreads';
ok "e\x[301]" eq "\x[e9]", 'graphemes compare after normalization';
nok "e\x[301]" lt "\x[e9]", '... in order comparisons too';
