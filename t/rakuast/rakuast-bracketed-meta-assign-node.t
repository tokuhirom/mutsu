use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# A bracketed metaoperator assignment (`[X+]=`, `[Z+]=`, `[R~]=`) is the typed
# compound-assignment node: `.AST` matches rakudo 2026.09, and the tree
# lowers to what the parsed program does (ADR-10723 Stage 2, #14948). This
# file passes under both mutsu and raku.

plan 10;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $cross = last-statement(Q[my @a; @a [X+]= 1]);
is $cross.infix.^name, 'RakuAST::MetaInfix::Assign', '[X+]= is a MetaInfix::Assign';
is $cross.infix.infix.^name, 'RakuAST::BracketedInfix', 'over the bracketed infix';
is $cross.infix.infix.infix.^name, 'RakuAST::MetaInfix::Cross', 'which holds the cross metaoperator';
is last-statement(Q[my @a; @a [R~]= 1]).infix.infix.infix.^name, 'RakuAST::MetaInfix::Reverse',
    '[R~]= holds the reverse metaoperator';
is last-statement(Q[my $x; say($x [X+]= 1)]).args.args[0].infix.infix.^name, 'RakuAST::BracketedInfix',
    'a bracketed metaoperator assignment in an argument';

# The trees run like the parsed program.
is EVAL(Q[my @a = 1, 2; @a [X+]= 10; @a.join(",")].AST), '11,12', '[X+]= on an array';
is EVAL(Q[my @a = 1, 2; @a [Z+]= 10, 20; @a.join(",")].AST), '11,22', '[Z+]= on an array';
is EVAL(Q[my $s = "a"; $s [R~]= "b"; $s].AST), 'ba', '[R~]= on a scalar';
is EVAL(Q[my @a = 1, 2; my $r = (@a [X*]= 3); @a.join(",")].AST), '3,6', 'in parentheses';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[my @a; @a [X+]= 1].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
