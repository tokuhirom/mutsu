use Test;
use experimental :rakuast;

plan 9;

# A hash composer or `%(...)` holding one entry and a trailing comma is a
# comma list of one in RakuAST, as rakudo parses it (#18444).

sub contents($code) { $code.AST.statements[0].expression }

is contents(Q|{:k,}|).expression.^name, 'RakuAST::ApplyListInfix', '{:k,}';
is contents(Q|{a => 1,}|).expression.^name, 'RakuAST::ApplyListInfix', '{a => 1,}';
is contents(Q|{:k}|).expression.^name, 'RakuAST::ColonPair::True', '{:k} is the pair';
is contents(Q|%(:k,)|).target.statements[0].expression.^name,
    'RakuAST::ApplyListInfix', '%(:k,)';

my $list = contents(Q|{:k,}|).expression;
is $list.infix.origin.from, 3, 'the comma is the infix';
is ($list.origin.from, $list.origin.to), (1, 4), 'the list spans the comma';

is Q|{:k,}|.AST.DEPARSE.trim, '{:k,}', 'DEPARSE';
is-deeply Q|{:k,}|.AST.EVAL, {:k}, 'EVAL';
is-deeply Q|{a => 1,}|.AST.EVAL, {a => 1}, 'EVAL of a fat arrow';
