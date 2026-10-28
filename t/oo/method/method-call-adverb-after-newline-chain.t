use Test;

plan 4;

# JavaScript::D3 (ecosystem): `$s\n    .subst(...):g` -- the adverb of a method
# call written on its own line after the invocant was dropped (as a `say`
# argument) or died "Two terms in a row" (in an initializer).
my $s = 'aXbX';
my $t = $s
    .subst('X', 'Y'):g;
is $t, 'aYbY', 'adverb on a leading-dot call, initializer';

is-deeply (join '|', $s
    .subst('X', 'Z'):g), 'aZbZ', 'adverb on a leading-dot call, listop argument';

my $u = $s
    .subst('X', 'W'):g
    .uc;
is $u, 'AWBW', 'adverb followed by another chained call';

is $s.subst('X', 'V'):g, 'aVbV', 'same-line form still works';
