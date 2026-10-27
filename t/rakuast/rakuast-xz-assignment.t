use Test;
use experimental :rakuast;

# `X=` and `Z=` apply the cross / zip metaoperator to rakudo's `Assignment`
# (`:item`) as a list infix. This file passes under both mutsu and raku.

plan 15;

sub shape(Str $src) {
    $src.AST.statements[*-1].expression.raku.subst(/\s+/, ' ', :g)
}

sub bracketed-assignment(Str $src, Str $meta) {
    my $infix = $src.AST.statements[*-1].expression.infix;
    $infix.^name eq 'RakuAST::BracketedInfix'
        && $infix.infix.^name eq "RakuAST::MetaInfix::$meta"
        && $infix.infix.infix.^name eq 'RakuAST::Assignment'
}

like shape(Q[my @a; @a X= 1]), /'MetaInfix::Cross.new( RakuAST::Assignment.new(:item) )'/,
    'X= is a cross over the assignment';
like shape(Q[my @a; @a Z= 1,2]), /'MetaInfix::Zip.new( RakuAST::Assignment.new(:item) )'/,
    'Z= is a zip over the assignment';
ok bracketed-assignment(Q[my @a; @a [Z=] 1,2], 'Zip'),
    '[Z=] is a bracketed zip over the assignment';
ok bracketed-assignment(Q[my @a; @a [X=] 1,2], 'Cross'),
    '[X=] is a bracketed cross over the assignment';
like shape(Q[my @a Z= 1,2]), /'MetaInfix::Zip.new( RakuAST::Assignment.new(:item) )' .* 'VarDeclaration::Simple'/,
    'a declaration Z= keeps the declaration as its first operand';
like shape(Q[my @a; say(@a X= 1)]), /'MetaInfix::Cross.new( RakuAST::Assignment.new(:item) )'/,
    'X= as a call argument';
like Q[my @a; @a X= 1 if 1].AST.statements[*-1].raku.subst(/\s+/, ' ', :g),
    /'MetaInfix::Cross.new( RakuAST::Assignment.new(:item) )' .* 'StatementModifier::If'/,
    'X= under a statement modifier';

is EVAL(Q[my @a = 1,2,3; @a Z= 7,8; @a].AST), [7, 8, 3], 'EVAL of Z= fills element-wise';
is EVAL(Q[my @a = 1,2; @a X= 5; @a].AST), [5, 5], 'EVAL of X=';
is EVAL(Q[my @a = 1,2,3; @a [Z=] 7,8; @a].AST), [7, 8, 3], 'EVAL of [Z=]';
is EVAL(Q[my @a = 1,2; @a [X=] 5; @a].AST), [5, 5], 'EVAL of [X=]';
like shape(Q[my @a; my @b = @a Z= 1,2]),
    /'MetaInfix::Zip.new( RakuAST::Assignment.new(:item) )' .* 'RakuAST::IntLiteral.new(1)' .* 'RakuAST::IntLiteral.new(2)'/,
    'Z= in an initializer keeps the complete comma-list RHS in its AST';
is EVAL(Q[my @a = 1,2; my @b = @a Z= 7,8; @b].AST), [7, 8], 'Z= consumes its full comma-list RHS in an initializer';

my @b = 1, 2, 3;
EVAL Q[@b Z= 9,8].AST;
is @b, [9, 8, 3], 'EVAL of Z= on an outer array';
my @c = 1, 2;
EVAL Q[@c X= 0].AST;
is @c, [0, 0], 'EVAL of X= on an outer array';
