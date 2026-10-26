use v6;
use Test;

# A declaration followed by a postfix in argument position
# (`is my $k.defined, False`) round-trips through RakuAST when it executes
# (MUTSU_RAKUAST=1): the converted tail must still begin with the declared
# variable, so the executing round trip carries no leaf inside it.
plan 4;

is my $k.defined, False, 'method on a declarator as a call argument';
is (my $m).defined, False, 'parenthesised declarator';
is-deeply (my @a.elems, 1), (0, 1), 'declarator postfix in a list';
is my $n.WHAT.^name, 'Any', 'postfix chain over a declarator';
