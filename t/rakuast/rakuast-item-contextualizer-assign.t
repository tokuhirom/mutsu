use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# An assignment to an item contextualizer (`$(EXPR) = v`), to an anonymous
# `$`, or to a double-underscore variable is the typed assignment node:
# `.AST` matches rakudo 2026.09, and the tree lowers to what the parsed
# program does (ADR-10723 Stage 2, #14948). This file passes under both mutsu
# and raku.

plan 13;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $item = last-statement(Q[my $s; $($s) = 42]);
is $item.left.^name, 'RakuAST::Contextualizer::Item', 'an item contextualizer target is kept';
is $item.infix.raku, 'RakuAST::Assignment.new(:item)', 'and it is an item assignment';
is last-statement(Q[my @a; @(@a) = 1, 2]).infix.raku, 'RakuAST::Assignment.new',
    'a list contextualizer target is a list assignment';
is last-statement(Q[my %h; $(%h<k>) = 9]).left.^name, 'RakuAST::Contextualizer::Item',
    'an item contextualizer around a subscript';

my $anon = last-statement(Q[$ = 5]);
is $anon.left.^name, 'RakuAST::VarDeclaration::Anonymous', 'an anonymous scalar target';
is $anon.infix.raku, 'RakuAST::Assignment.new(:item)', 'is an item assignment';

is last-statement(Q[my $__c; $__c = 3]).left.raku, Q[RakuAST::Var::Lexical.new("\$__c")],
    'a double-underscore variable target is the variable as written';

# The trees run like the parsed program.
is EVAL(Q[my $s; $($s) = 42; $s].AST), 42, 'an item contextualizer writes through';
is EVAL(Q[my %h; $(%h<k>) = 9; %h<k>].AST), 9, 'through a hash element';
is EVAL(Q[my @a; $(@a[0]) = 1, 2; @a.elems].AST), 1, 'the comma stays outside the item assignment';
is EVAL(Q[my $__c; $__c = 3; $__c].AST), 3, 'a double-underscore variable';
is EVAL(Q[sub f { $ = 5 }; f()].AST), 5, 'an anonymous scalar';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[my $s; $($s) = 42].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';
