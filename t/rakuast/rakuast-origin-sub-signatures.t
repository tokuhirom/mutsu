use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a sub-signature written as a
# pointy block's or a regex declaration's parameter (`($a, :$b)`): the
# parameter spans its parentheses, its `Signature` the text inside them, and
# each inner parameter its own text.

plan 2;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }

my $b = Q[my $f = -> Int $x, ($a, :$b) { };].AST.statements[0].expression.initializer.expression;
my $p = $b.signature.parameters[1];
is (o($b.signature), o($p), o($p.sub-signature),
    $p.sub-signature.parameters.map({ o($_) ~ '/' ~ o(.target) }).join(' ')).join(' '),
    '11-28 19-28 20-27 20-22/20-22 24-27/25-27', 'a pointy block parameter';
my $g = Q[grammar G { token w(($a, $b)) { a } }].AST.statements[0].expression.body.body.statement-list.statements[0].expression;
is o($g.signature.parameters[0]) ~ ' ' ~ o($g.signature.parameters[0].sub-signature), '20-28 21-27',
    'a token parameter';
