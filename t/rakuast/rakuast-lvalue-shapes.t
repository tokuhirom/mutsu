use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# The left side of an assignment keeps the shape rakudo 2026.09 parses it
# into: a prefix, ternary or capture variable is the node itself, a call is
# the call, and a parenthesised variable stays a `Circumfix::Parentheses`
# (which makes it a list assignment). This file passes under both mutsu and
# raku.

plan 19;

sub shape(Str $src) {
    my $assign = $src.AST.statements[*-1].expression;
    $assign.left.^name ~ ' ' ~ ($assign.infix.raku.contains(':item') ?? 'item' !! 'list')
}

is shape(Q[my $x; -$x = 5]), 'RakuAST::ApplyPrefix item', 'a prefix target';
is shape(Q[my ($x, $y); $y ?? 1 !! $x = 2]), 'RakuAST::Ternary item', 'a ternary target';
is shape(Q[my $c; $<c> = $0]), 'RakuAST::Var::NamedCapture item', 'a named capture target';
is shape(Q[my ($x, $y); (1 ?? $x !! $y) = 9]), 'RakuAST::Circumfix::Parentheses list',
    'a parenthesised ternary target';
is shape(Q[my $x; (-$x) = 5]), 'RakuAST::Circumfix::Parentheses list',
    'a parenthesised prefix target';
is shape(Q[sub c is rw { state $z }; c() = 5]), 'RakuAST::Call::Name list', 'a call target';
is shape(Q[my $c; $c() = 5]), 'RakuAST::ApplyPostfix list', 'a term call target';
is shape(Q[my $x; ($x) = 1]), 'RakuAST::Circumfix::Parentheses list', 'a grouped scalar';
is shape(Q[my @a; (@a) = 1, 2]), 'RakuAST::Circumfix::Parentheses list', 'a grouped array';
is shape(Q[my %h; (%h) = a => 1]), 'RakuAST::Circumfix::Parentheses list', 'a grouped hash';
is Q[my $x; say(($x) = 1)].AST.statements[1].expression.args.args[0].left.^name,
    'RakuAST::Circumfix::Parentheses', 'a grouped scalar in an argument list';

is Q[my $x; ($x) = 1].AST.statements[1].expression.left.raku, Q:to/END/.chomp,
RakuAST::Circumfix::Parentheses.new(
  RakuAST::SemiList.new(
    RakuAST::Statement::Expression.new(
      expression => RakuAST::Var::Lexical.new("\$x")
    )
  )
)
END
    'the parentheses hold the variable';

# The trees evaluate like the source they came from.
is EVAL(Q[my $x; ($x) = 7; $x].AST), 7, 'a grouped scalar';
is EVAL(Q[my @a; (@a) = 1, 2; @a.elems].AST), 2, 'a grouped array takes the list';
is EVAL(Q[my %h; (%h) = a => 1; %h<a>].AST), 1, 'a grouped hash takes the pair';
is EVAL(Q[my $x; my $y = 0; $y ?? 1 !! $x = 2; $x].AST), 2,
    'a ternary target assigns to the chosen branch';
is EVAL(Q[sub c is rw { state $z }; c() = 5; c()].AST), 5, 'a call target';

# Rebuilding a tree from its `.raku` gives the same tree.
for Q[my $x; ($x) = 1], Q[my ($x, $y); $y ?? 1 !! $x = 2] -> $src {
    my $ast = $src.AST;
    is EVAL($ast.raku).raku, $ast.raku, "round trip: $src";
}
