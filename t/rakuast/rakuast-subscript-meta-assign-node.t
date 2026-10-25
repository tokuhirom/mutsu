use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# A bracketed metaoperator assignment to an element (`%h<k> [R//]= v`,
# `@a[i] [X+]= v`) is the typed compound-assignment node, like the one on a
# variable: `.AST` matches rakudo 2026.09, and the tree lowers to what the
# parsed program does (ADR-10723 Stage 2, #14948). This file passes under both
# mutsu and raku.

plan 11;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $rev = last-statement(Q[my %r; %r<x> [R//]= "y"]);
is $rev.^name, 'RakuAST::ApplyInfix', '[R//]= on an element is an infix application';
is $rev.left.postfix.^name, 'RakuAST::Postcircumfix::LiteralHashIndex', 'whose left side is the element';
is $rev.infix.infix.infix.^name, 'RakuAST::MetaInfix::Reverse', 'under the reversed bracketed infix';
is last-statement(Q[my @a; @a[1] [X+]= 2]).infix.infix.infix.^name, 'RakuAST::MetaInfix::Cross',
    '[X+]= on an array element';
is last-statement(Q[my @a; @a[1] [+]= 2]).infix.infix.^name, 'RakuAST::BracketedInfix',
    '[+]= on an array element';

# The trees run like the parsed program.
is EVAL(Q[my %r; %r<x> [R//]= "y"; %r<x>].AST), 'y', '[R//]= assigns when the element is undefined';
is EVAL(Q[my %r = x => "old"; %r<x> [R//]= Str; %r<x>].AST), 'old', '[R//]= keeps the element when the right side is undefined';
is EVAL(Q[my %h = n => 5; %h<n> [+]= 10; %h<n>].AST), 15, '[+]= on a hash element';
is EVAL(Q[my @a = 1, 3; @a[1] [*]= 10; @a[1]].AST), 30, '[*]= on an array element';
is EVAL(Q[my %r; my $v = (%r<x> [R//]= "y"); $v].AST), 'y', 'in parentheses';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[my %r; %r<x> [R//]= "y"].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
