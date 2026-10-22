use Test;

# `|$pair` of a Pair that carries a role mixin is still a named argument.
# CSS::Properties' calc() passes `|$expr` where the CSS grammar actions typed
# the Pair (`Pair but Role`) (#14158). Expected values are the `raku` output.

plan 6;

role R { method type { 'x' } }
sub one(:$func!) { "named:" ~ $func.elems }
sub slurpy(*%n) { %n.keys.sort.join(',') }

my $p = (func => {a => 1, b => 2}) but R;
is $p.WHAT.^name, 'Pair+{R}', 'the pair carries the role';
is one(|$p), 'named:2', 'a mixed-in pair is slipped as a named argument';
is slurpy(|$p), 'func', 'and reaches a slurpy named parameter by its key';

my $q = (func => {a => 1}); $q does R;
is one(|$q), 'named:1', 'also after a does mixin';

sub typed(Pair $x) { one |$x }
is typed($p), 'named:2', 'through a typed parameter';

class K { multi method m($*c: Pair $x, :$*r = 1) { one |$x } }
is K.new.m($p), 'named:2', 'through a multi method with a dynamic invocant';
