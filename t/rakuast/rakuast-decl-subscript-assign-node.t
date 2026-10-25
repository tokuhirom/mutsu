use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# `my @a.[2] = 42` is `(my @a).[2] = 42`: the assignment to the subscript of
# the just-declared variable is the typed assignment node. `.AST` matches
# rakudo 2026.09 (the subscript over the declaration, the value as its
# `assignee`, an `Assignment` infix over a `{…}` subscript), and the tree lowers
# to what the parsed program does (ADR-10723 Stage 2, #14948). This file passes
# under both mutsu and raku.

plan 10;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $pos = last-statement(Q[my @a.[2] = 42]);
is $pos.operand.^name, 'RakuAST::VarDeclaration::Simple', 'the subscript applies to the declaration';
is $pos.postfix.^name, 'RakuAST::Postcircumfix::ArrayIndex', 'an array index';
is $pos.postfix.assignee.^name, 'RakuAST::IntLiteral', 'which takes the value as its assignee';
is last-statement(Q[my %h.<k> = 5]).postfix.assignee.^name, 'RakuAST::IntLiteral',
    'a literal hash index takes it too';
is last-statement(Q[my %h.{"x"} = 9]).infix.^name, 'RakuAST::Assignment',
    'a {…} subscript keeps the assignment infix';

# The trees run like the parsed program.
is-deeply EVAL(Q[my @a.[2] = 42; @a].AST), [Any, Any, 42], 'my @a.[2] = 42';
is-deeply EVAL(Q[my %h.<k> = 5; %h].AST), {k => 5}, 'my %h.<k> = 5';
is-deeply EVAL(Q[my %h.{"x"} = 9; %h].AST), {x => 9}, 'my %h.{"x"} = 9';
is-deeply EVAL(Q[my @a.[1] = 7; @a[3] = 9; @a].AST), [Any, 7, Any, 9], 'the declaration is in scope after';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[my @a.[2] = 42].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
