use v6;
use Test;
use experimental :rakuast;

plan 12;

# A multi method whose parameters are RakuAST types dispatches a node by the
# measured RakuAST hierarchy: the narrowest class wins, `:D` takes a node,
# `:U` takes a type object.
class Probe {
    multi method m(RakuAST::Node:D $n)       { 'Node:D' }
    multi method m(RakuAST::Literal:D $n)    { 'Literal:D' }
    multi method m(RakuAST::IntLiteral:D $n) { 'IntLiteral:D' }
    multi method m(RakuAST::StrLiteral:D $n) { 'StrLiteral:D' }
    multi method m(RakuAST::Node:U $n)       { 'Node:U' }
    multi method m(RakuAST::IntLiteral:U $n) { 'IntLiteral:U' }
    multi method m(Mu:D $x)                  { 'Mu:D' }
}

my $p = Probe.new;
my $int = RakuAST::IntLiteral.new(42);
my $str = RakuAST::StrLiteral.new('x');
my $stmt = RakuAST::Statement::Expression.new(expression => $int);

for 1..2 -> $round {
    is $p.m($int), 'IntLiteral:D', "round $round: the narrowest class wins";
    is $p.m($str), 'StrLiteral:D', "round $round: a sibling class";
    is $p.m($stmt), 'Node:D', "round $round: a class with only the root candidate";
    is $p.m(RakuAST::IntLiteral), 'IntLiteral:U', "round $round: a type object takes :U";
    is $p.m(RakuAST::Statement::Expression), 'Node:U', "round $round: a wider type object";
}

is $p.m(42), 'Mu:D', 'a non-node value takes the Mu:D candidate';
is $p.m('s'), 'Mu:D', 'a Str value takes the Mu:D candidate';
