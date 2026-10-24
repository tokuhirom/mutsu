use Test;
use experimental :rakuast;

plan 4;

# RakuAST model classes (abstract ones included) are CORE type names, so a
# `when` matcher naming one is a complete term and does not gobble the block.
sub kind($node) {
    given $node {
        when RakuAST::Literal { 'literal' }
        when RakuAST::StatementList { 'statements' }
        when RakuAST::Termish { 'termish' }
        default { 'other' }
    }
}

is kind(RakuAST::IntLiteral.new(1)), 'literal', 'abstract RakuAST::Literal';
is kind(RakuAST::StatementList.new), 'statements', 'concrete RakuAST::StatementList';
is kind(42), 'other', 'a non-node falls through';

throws-like 'given 1 { when RakuAST::NoSuchClass { 1 } }', X::Comp::Group,
  'an undeclared RakuAST name still gobbles the block';
