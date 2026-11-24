use experimental :rakuast;
use Test;

# Substitutions used as infix operands retain the ranges of their parts.
plan 9;

sub range($node) { $node.origin.from ~ '-' ~ $node.origin.to }

my $literal = Q[my $s = "a"; $s ~~ s{a} = q{$x}].AST.statements[1].expression.args.args[1];
is $literal.^name, 'RakuAST::Substitution', 'the right operand is a substitution';
is range($literal), '19-31', 'the substitution has its source range';
is range($literal.pattern), '21-22', 'the pattern has its source range';
is range($literal.infix), '24-25', 'the assignment has its source range';

my $samespace = Q[my $s = "a b"; $s ~~ ss[a b] = "c d"].AST.statements[1].expression.args.args[1];
is range($samespace), '21-36', 'a samespace substitution has its source range';
is range($samespace.pattern), '24-27', 'its pattern has its source range';
is range($samespace.infix), '29-30', 'its assignment has its source range';

my $adverb = Q[my $s = "a"; $s ~~ s:g/a/b/].AST.statements[1].expression.args.args[1];
is range($adverb), '19-27', 'a substitution with an adverb has its source range';
is range($adverb.pattern), '23-24', 'the adverb does not shift the pattern range';
