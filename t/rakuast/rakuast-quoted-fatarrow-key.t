use Test;
use experimental :rakuast;

plan 12;

# A fat arrow with a quoted string key is an `ApplyInfix` over a `QuotedString`
# (a bare `a => 2` is a `FatArrow`); `.DEPARSE` keeps the quotes, which a key with
# a space needs to be valid Raku at all (#14845).
for 'my $p = ("a" => 2)'      => 'my $p = ("a" => 2)',
    "my \$p = ('a' => 2)"     => 'my $p = ("a" => 2)',
    'my $p = ("a b" => 2)'    => 'my $p = ("a b" => 2)',
    'my $p = ("a" => 2).Bag'  => 'my $p = ("a" => 2).Bag',
    'my $p = (a => 2)'        => 'my $p = (a => 2)' -> (:key($src), :value($want)) {
    is $src.AST.DEPARSE.trim, $want, "DEPARSE of $src";
}

my $infix = 'my $p = ("a" => 2)'.AST.find-nodes(RakuAST::ApplyInfix)[0];
isa-ok $infix, RakuAST::ApplyInfix, 'quoted key is an ApplyInfix';
is $infix.infix.operator, '=>', 'over the => infix';
isa-ok $infix.left, RakuAST::QuotedString, 'with a QuotedString key';
is 'my $p = (a => 2)'.AST.find-nodes(RakuAST::FatArrow).elems, 1, 'a bare key is a FatArrow';
is 'my $p = ("a" => 2)'.AST.find-nodes(RakuAST::FatArrow).elems, 0, 'a quoted key is not';

use MONKEY-SEE-NO-EVAL;
is-deeply EVAL('("a b" => 2)'.AST), ("a b" => 2), 'the pair still evaluates';
is-deeply EVAL('my %h = "x" => 1, "y z" => 2; %h.sort.List'.AST), (("x" => 1), ("y z" => 2)), 'in a hash assignment';
