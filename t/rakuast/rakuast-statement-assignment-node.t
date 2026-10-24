use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# An assignment statement to a variable is the typed assignment node: `.AST`
# matches rakudo 2026.09, and the tree lowers to what the parsed program does
# (ADR-10723 Stage 2, #14948). This file passes under both mutsu and raku.

plan 11;

sub statement(Str $src) { $src.AST.statements[*-1].expression }

is statement(Q[my &f; &f = { 1 }]).left.raku, Q[RakuAST::Var::Lexical.new("\&f")],
    'a code variable target';
is statement(Q[my &f; &f = { 1 }]).infix.raku, 'RakuAST::Assignment.new',
    'is a list assignment';

my $tail = statement(Q[my $x; $x = 1 and say 2]);
is $tail.infix.raku, 'RakuAST::Infix.new("and")', 'a loose and takes the assignment as its left side';
is $tail.left.infix.raku, 'RakuAST::Assignment.new(:item)', 'which is an item assignment';

my $block = Q[my $b = { $^x = 9 }].AST.statements[0].expression.initializer.expression;
my $placeholder = $block.body.statement-list.statements[0].expression;
is $placeholder.left.^name, 'RakuAST::VarDeclaration::Placeholder::Positional',
    'a placeholder target declares the placeholder';
is $placeholder.infix.raku, 'RakuAST::Assignment.new(:item)', 'and it is an item assignment';

# The trees run like the parsed program.
is EVAL(Q[my $x; $x = 1, 2; $x].AST), 1, 'a scalar takes the first item';
is EVAL(Q[my @a; @a = 1, 2; @a.elems].AST), 2, 'an array takes the list';
is EVAL(Q[my &f; &f = { 42 }; f()].AST), 42, 'a code variable';
is EVAL(Q[my $x; my $r = 0; $x = 0 or $r = 5; $r].AST), 5, 'a loose or runs after the assignment';
throws-like { EVAL(Q[my $b = { $^x = 9 }; $b(1)].AST) }, Exception,
    'a placeholder is read-only';
