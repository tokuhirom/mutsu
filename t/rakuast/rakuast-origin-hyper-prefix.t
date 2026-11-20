use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the `MetaPrefix::Hyper` of a
# hyper prefix (`-<< @x`, `|« @x`) runs over the operator and its `<<` / `«`,
# and the `Prefix` it holds over the operator.

plan 4;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }
sub shape($src) {
    my $a = $src.AST.statements[1].expression;
    (o($a), o($a.prefix), o($a.prefix.prefix), o($a.operand)).join(' ')
}

is shape(Q[my @x; |<< @x]), '7-13 7-10 7-8 11-13', 'a hyper slip';
is shape(Q[my @x; -<< @x]), '7-13 7-10 7-8 11-13', 'a hyper negation';
is shape(Q[my @x; -« @x]), '7-12 7-9 7-8 10-12', 'the Unicode marker';
is shape(Q[my @x; !<<@x]), '7-12 7-10 7-8 10-12', 'no space before the operand';
