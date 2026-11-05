use Test;

# Terms that look alike in a modified statement and in its modifier
# (`$n = $n.abs if $n`) each get their own range, as in rakudo 2026.09
# (#15780).

plan 9;

use experimental :rakuast;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }
sub stmt(Str $src) { $src.AST.statements[1] }

my $a = stmt('my $n = 1; $n = $n if $n;');
is rng($a.expression.left), "11-13", 'assignment target';
is rng($a.expression.right), "16-18", 'assignment source (equal to target and condition)';
is rng($a.condition-modifier.expression), "22-24", 'condition';

my $b = stmt('my $n = 1; $n = $n.abs if $n;');
is rng($b.expression.right.operand), "16-18", 'method call invocant';
is rng($b.condition-modifier.expression), "26-28", 'a bare variable condition';

my $c = stmt('my $n = 1; $n = $n.abs if $n.WHAT;');
is rng($c.expression.right.operand), "16-18", 'invocant of the source';
is rng($c.condition-modifier.expression), "26-33", 'method call condition';
is rng($c.condition-modifier.expression.operand), "26-28", 'its invocant';
is rng($c.condition-modifier.expression.postfix), "29-33", 'its method name';
