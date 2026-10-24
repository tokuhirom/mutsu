use Test;
use experimental :rakuast;

# `Assignment.new(:item)` follows rakudo 2026.09's `$*LEFTSIGIL`: the first
# variable of an expression sets it, a postfix makes it `@`, an assignment
# reads it and clears it, and a nested expression (an argument list) has its
# own. And `@a X= 1` is a cross metaoperator over the assignment. This file
# passes under both mutsu and raku.

plan 13;

sub item-flags(Str $src) {
    $src.AST.statements[*-1].expression.raku.comb(/'Assignment.new' ['(:item)']?/).join(' ')
}

is item-flags(Q[my $x; $x = 1]), 'Assignment.new(:item)', 'a scalar target';
is item-flags(Q[my @a; @a = 1]), 'Assignment.new', 'an array target';
is item-flags(Q[my &f; &f = 5]), 'Assignment.new', 'a code target';
is item-flags(Q[my $x; 1 andthen $x = 2]), 'Assignment.new(:item)',
    'the first variable decides, wherever it is';
is item-flags(Q[my $x; $x.say andthen $x = 2]), 'Assignment.new',
    'a postfix makes the expression list-like';
is item-flags(Q[my $x; $x.foo = 5]), 'Assignment.new', 'a method call target';
is item-flags(Q[my ($a, $b); $a = $b = 1]), 'Assignment.new(:item) Assignment.new(:item)',
    'an assignment clears the sigil for the next variable';
is item-flags(Q[my @a; my $x; @a, $x = 2]), 'Assignment.new', 'an earlier array';
is item-flags(Q[my @a; my $x; @a.push: $x = 1]), 'Assignment.new(:item)',
    'an argument list is an expression of its own';
is item-flags(Q[my $x; say 1, $x = 2]), 'Assignment.new(:item)', 'a listop argument';

is Q[my @foo; @foo X= 1].AST.statements[1].expression.raku, Q:to/END/.chomp, 'X= is a cross over the assignment';
RakuAST::ApplyListInfix.new(
  infix    => RakuAST::MetaInfix::Cross.new(
    RakuAST::Assignment.new(:item)
  ),
  operands => (
    RakuAST::Var::Lexical.new("\@foo"),
    RakuAST::IntLiteral.new(1),
  )
)
END
is EVAL(Q[my @b = 1, 2, 3; @b X= 7; @b].AST), [7, 7, 7], 'EVAL of X=';
my @c = 1, 2;
EVAL Q[@c X= 5].AST;
is @c, [5, 5], 'EVAL of X= on an outer array';
