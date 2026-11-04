use Test;

# A hyper compound assignment (`@a »~=» @b`) is
# `ApplyInfix(left, MetaInfix::Assign(MetaInfix::Hyper(Infix "~")), right)` in
# rakudo's `.AST` (#16381), not a `MetaInfix::Hyper` over `Infix "~="`. The
# tree lowers back to the hyper operator, and a hyper comparison that merely
# ends in `=` (`»<=»`) keeps its plain shape.

plan 7;

use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

sub flat(Str $s) { $s.subst(/\s+/, ' ', :g) }
sub infix-of(Str $src) { $src.AST.statements[0].expression.infix }

my $i = infix-of('1 »~=» 2');
is $i.^name, 'RakuAST::MetaInfix::Assign', 'the infix is a MetaInfix::Assign';
is $i.infix.^name, 'RakuAST::MetaInfix::Hyper', 'around the hyper operator';
is flat($i.infix.infix.raku), 'RakuAST::Infix.new("~")', 'over the base operator';
is flat($i.raku), 'RakuAST::MetaInfix::Assign.new( RakuAST::MetaInfix::Hyper.new( infix => RakuAST::Infix.new("~"), dwim-right => True ) )', 'and prints as rakudo does';

my $c = infix-of('1 »<=» 2');
is $c.^name, 'RakuAST::MetaInfix::Hyper', 'a hyper comparison keeps the plain hyper shape';

is EVAL(Q[my @a = 1, 2, 3; @a »+=» 10; my @b = <x y>; @b »~=» "!"; @a.raku ~ " " ~ @b.raku].AST),
    '[11, 12, 13] ["x!", "y!"]', 'EVAL of the tree assigns element-wise';
is EVAL(Q[my @c = 5, 6; @c »*=» 2; @c.raku].AST), '[10, 12]', 'another operator';
