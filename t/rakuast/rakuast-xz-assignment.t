use Test;
use experimental :rakuast;

# `X=` and `Z=` apply the cross / zip metaoperator to rakudo's `Assignment`
# (`:item`) as a list infix. This file passes under both mutsu and raku.

plan 9;

sub shape(Str $src) {
    $src.AST.statements[*-1].expression.raku.subst(/\s+/, ' ', :g)
}

like shape(Q[my @a; @a X= 1]), /'MetaInfix::Cross.new( RakuAST::Assignment.new(:item) )'/,
    'X= is a cross over the assignment';
like shape(Q[my @a; @a Z= 1,2]), /'MetaInfix::Zip.new( RakuAST::Assignment.new(:item) )'/,
    'Z= is a zip over the assignment';
like shape(Q[my @a Z= 1,2]), /'MetaInfix::Zip.new( RakuAST::Assignment.new(:item) )' .* 'VarDeclaration::Simple'/,
    'a declaration Z= keeps the declaration as its first operand';
like shape(Q[my @a; say(@a X= 1)]), /'MetaInfix::Cross.new( RakuAST::Assignment.new(:item) )'/,
    'X= as a call argument';
like Q[my @a; @a X= 1 if 1].AST.statements[*-1].raku.subst(/\s+/, ' ', :g),
    /'MetaInfix::Cross.new( RakuAST::Assignment.new(:item) )' .* 'StatementModifier::If'/,
    'X= under a statement modifier';

is EVAL(Q[my @a = 1,2,3; @a Z= 7,8; @a].AST), [7, 8, 3], 'EVAL of Z= fills element-wise';
is EVAL(Q[my @a = 1,2; @a X= 5; @a].AST), [5, 5], 'EVAL of X=';

my @b = 1, 2, 3;
EVAL Q[@b Z= 9,8].AST;
is @b, [9, 8, 3], 'EVAL of Z= on an outer array';
my @c = 1, 2;
EVAL Q[@c X= 0].AST;
is @c, [0, 0], 'EVAL of X= on an outer array';
