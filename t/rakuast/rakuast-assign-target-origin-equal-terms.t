use Test;
use experimental :rakuast;

plan 6;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }

# The assignment target `$n` is a term of its own: it must not take the range
# of a later equal `$n`, which left the condition's invocant without one.
my $s = 'my $n = 1; $n = $n.abs if $n.WHAT;'.AST.statements[1];
my $c = $s.condition-modifier.expression;
is rng($c), '26-33', 'the condition call';
is rng($c.operand), '26-28', 'the condition invocant';
is rng($c.postfix), '29-33', 'the condition postfix';

my $t = 'my $n = 1; $n = 2 if $n.WHAT;'.AST.statements[1];
is rng($t.condition-modifier.expression.operand), '21-23', 'a literal source';

my $u = 'my $n = 1; $n = $n.abs;'.AST.statements[1];
is rng($u.expression.left), '11-13', 'the assignment target';
is rng($u.expression.right.operand), '16-18', 'the source invocant';
