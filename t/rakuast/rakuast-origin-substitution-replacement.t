use experimental :rakuast;
use Test;

# Rakudo gives a literal replacement quote the range of its contents and
# leaves the single StrLiteral segment without an origin.
plan 8;

sub range($node) { $node.origin.from ~ '-' ~ $node.origin.to }

my $plain = Q[s{a}="b"].AST.statements[0].expression.replacement;
is range($plain), '6-7', 'a quoted replacement covers its contents';
ok !$plain.segments[0].origin.defined, 'its literal segment has no origin';

my $single = Q[s:i:g{ a } = 'c'].AST.statements[0].expression.replacement;
is range($single), '14-15', 'a single-quoted replacement covers its contents';
ok !$single.segments[0].origin.defined, 'the single-quoted segment has no origin';

my $nested = Q[my $s = "a"; $s ~~ s{a} = q{$x}].AST.statements[1].expression.args.args[1].replacement;
is range($nested), '28-30', 'a q-quoted replacement inside an infix operand';
ok !$nested.segments[0].origin.defined, 'the q-quoted segment has no origin';

my $samespace = Q[my $s = "a b"; $s ~~ ss[a b] = "c d"].AST.statements[1].expression.args.args[1].replacement;
is range($samespace), '32-35', 'a samespace replacement covers its contents';
ok !$samespace.segments[0].origin.defined, 'the samespace segment has no origin';
