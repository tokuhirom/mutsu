use experimental :rakuast;
use Test;

# Rakudo 2026.11 gives a literal replacement quote its full source range
# and its single StrLiteral segment the range of the contents.
plan 8;

sub range($node) { $node.origin.from ~ '-' ~ $node.origin.to }

my $plain = Q[s{a}="b"].AST.statements[0].expression.replacement;
is range($plain), '5-8', 'a quoted replacement includes its delimiters';
is range($plain.segments[0]), '6-7', 'its literal segment covers the contents';

my $single = Q[s:i:g{ a } = 'c'].AST.statements[0].expression.replacement;
is range($single), '13-16', 'a single-quoted replacement includes its delimiters';
is range($single.segments[0]), '14-15', 'the single-quoted segment covers the contents';

my $nested = Q[my $s = "a"; $s ~~ s{a} = q{$x}].AST.statements[1].expression.args.args[1].replacement;
is range($nested), '26-31', 'a q-quoted replacement inside an infix operand';
is range($nested.segments[0]), '28-30', 'the q-quoted segment covers the contents';

my $samespace = Q[my $s = "a b"; $s ~~ ss[a b] = "c d"].AST.statements[1].expression.args.args[1].replacement;
is range($samespace), '31-36', 'a samespace replacement includes its delimiters';
is range($samespace.segments[0]), '32-35', 'the samespace segment covers the contents';
