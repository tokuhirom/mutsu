use v6.e.PREVIEW;
use Test;

# `has-compile-time-value` on any node, as rakudo 2026.09 answers it: the
# CompileTimeValue role classes are True, a few classes compute it from their
# children (a list that is not exactly one statement answers the Int 0), and
# every other node is False.

plan 24;

my $i = RakuAST::IntLiteral.new(1);
my $v = RakuAST::Var::Lexical.new('$x');
my sub se($e) { RakuAST::Statement::Expression.new(expression => $e) }
my sub has($n) { $n.has-compile-time-value.raku }

is has($i), 'Bool::True', 'a literal';
is has(RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("Int"))), 'Bool::True', 'a type';
is has(RakuAST::Block.new), 'Bool::True', 'a block';
is has(RakuAST::ColonPair::True.new('a')), 'Bool::True', 'a True colonpair';
is has($v), 'Bool::False', 'a variable';
is has(RakuAST::Name.from-identifier("foo")), 'Bool::False', 'a name';
is has(RakuAST::Call::Name.new(name => RakuAST::Name.from-identifier("f"))), 'Bool::False', 'a call';
is has(RakuAST::Term::Name.new(RakuAST::Name.from-identifier("Int"))), 'Bool::False', 'a term name';
is has(RakuAST::ApplyInfix.new(left => $i, infix => RakuAST::Infix.new('+'), right => $i)), 'Bool::False', 'an infix';

is has(RakuAST::QuotedString.new(segments => (RakuAST::StrLiteral.new("a"),))), 'Bool::True',
    'a quoted string of literal segments';
is has(RakuAST::QuotedString.new(segments => ())), 'Bool::True', 'an empty quoted string';
is has(RakuAST::QuotedString.new(segments => (se($v),))), 'Bool::False', 'an interpolating one';

is has(se($i)), 'Bool::True', 'a statement over a literal';
is has(se($v)), 'Bool::False', 'a statement over a variable';
is has(RakuAST::Statement::Expression.new(expression => $i,
        condition-modifier => RakuAST::StatementModifier::If.new($i))), '0',
    'a statement with a modifier answers the Int 0';

is has(RakuAST::StatementList.new), '0', 'an empty list answers 0';
is has(RakuAST::StatementList.new(se($i))), 'Bool::True', 'one literal statement';
is has(RakuAST::StatementList.new(se($v))), 'Bool::False', 'one variable statement';
is has(RakuAST::StatementList.new(se($i), se($i))), '0', 'two statements answer 0';
is has(RakuAST::SemiList.new(se($i))), 'Bool::True', 'a semilist of one literal';

is has(RakuAST::Circumfix::Parentheses.new(RakuAST::SemiList.new(se($i)))), 'Bool::True',
    'parentheses over one literal';
is has(RakuAST::Circumfix::Parentheses.new(RakuAST::SemiList.new(se($i), se($i)))), '0',
    'parentheses over two statements answer 0';
is has(RakuAST::ColonPair::Value.new(key => "a", value => $i)), 'Bool::True', 'a valued colonpair of a literal';
is has(RakuAST::ColonPair::Value.new(key => "a", value => $v)), 'Bool::False', 'and of a variable';
