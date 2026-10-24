use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# A `.=` assignment is the typed compound-assignment node: `.AST` matches
# rakudo 2026.09, and the tree lowers to what the parsed program does
# (ADR-10723 Stage 2, #14948). This file passes under both mutsu and raku.

plan 15;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $dotty = last-statement(Q[my $x; $x .= uc]);
is $dotty.^name, 'RakuAST::ApplyDottyInfix', '.= is an ApplyDottyInfix';
is $dotty.infix.^name, 'RakuAST::DottyInfix::CallAssign', 'over the call-assign infix';
is $dotty.right.name.canonicalize, 'uc', 'whose right side is the method call';
is last-statement(Q[my %h; %h<a> .= uc]).left.^name, 'RakuAST::ApplyPostfix', 'a subscript target as written';
is last-statement(Q[my $x; say($x .= uc)]).args.args[0].^name, 'RakuAST::ApplyDottyInfix',
    'a .= in an argument';

my $hyper = last-statement(Q[my @a; @a>>.=uc]);
is $hyper.right.^name, 'RakuAST::MetaPostfix::Hyper', 'a hyper .= is the hyper of the call';
is $hyper.right.postfix.dispatch, '.=', 'whose method carries the .= dispatch';

# The trees run like the parsed program.
is EVAL(Q[my $x = "a"; $x .= uc; $x].AST), 'A', 'a scalar';
is EVAL(Q[my @a = 3, 1; @a .= sort; @a.join(",")].AST), '1,3', 'an array';
is EVAL(Q[my %h; %h<a> = "x"; %h<a> .= uc; %h<a>].AST), 'X', 'a subscript';
is EVAL(Q[my @g = <a b>; given @g { .=reverse }; @g.join].AST), 'ba', 'the topic writes through to its container';
is EVAL(Q[my $x = -3; $x .= &abs; $x].AST), 3, 'a routine as the method';
is EVAL(Q[my $x = "a"; my $r = ($x .= uc); "$x $r"].AST), 'A A', 'a .= yields the new value';
is EVAL(Q[my @a = <a b>; @a>>.=uc; @a.join].AST), 'AB', 'a hyper .= on an array';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[my $x; $x .= uc].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
