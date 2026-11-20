use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: block arguments of a regex
# call assertion (`<word(-> $x { 1 }, :a{ 2 })>`), with their signatures,
# bodies and statements.

plan 2;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }

my $a = Q[my $r = /<word(-> $x { 1 }, :a{ 2 })>/;].AST.statements[0].expression.initializer.expression.body.args.args;
my $p = $a[0];
is (o($p), o($p.signature), o($p.body), o($p.body.statement-list.statements[0])).join(' '),
    '15-26 18-20 21-26 23-24', 'a pointy block argument';
is (o($a[1]), o($a[1].value), o($a[1].value.body.statement-list.statements[0])).join(' '),
    '28-35 30-35 32-33', 'a block-valued colonpair argument';
