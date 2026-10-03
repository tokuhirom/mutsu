use v6;
use experimental :rakuast;
use Test;

# Binding (`:=`) across the RakuAST boundary. Measured against rakudo 2026.09,
# `my $x := EXPR` is a `VarDeclaration::Simple` whose initializer is an
# `Initializer::Bind`, and a bind to an existing variable is an `ApplyInfix`
# with a plain `:=` infix. Both EVAL back to a binding, not an assignment.

plan 18;

# --- declarations render an Initializer::Bind -------------------------------
my $scalar = Q|my $x := 42|.AST.statements[0].expression;
is $scalar.^name, 'RakuAST::VarDeclaration::Simple', 'a bound scalar declaration';
is $scalar.initializer.^name, 'RakuAST::Initializer::Bind', 'with an Initializer::Bind';
is $scalar.initializer.expression.^name, 'RakuAST::IntLiteral', 'holding the right-hand side';

is Q|my $y = 1; my $x := $y|.AST.statements[1].expression.initializer.^name,
    'RakuAST::Initializer::Bind', 'a scalar bound to a variable';
is Q|sub f { 1 }; my $x := f()|.AST.statements[1].expression.initializer.^name,
    'RakuAST::Initializer::Bind', 'a scalar bound to a call';
is Q|my @a = 1; my @b := @a|.AST.statements[1].expression.initializer.^name,
    'RakuAST::Initializer::Bind', 'an array declaration';
is Q|my %s; my %h := %s|.AST.statements[1].expression.initializer.^name,
    'RakuAST::Initializer::Bind', 'a hash declaration';
is Q|my $x = 42|.AST.statements[0].expression.initializer.^name,
    'RakuAST::Initializer::Assign', 'an assignment stays an Initializer::Assign';

# --- a bind to an existing variable -----------------------------------------
my $infix = Q|my $y; $y := 3|.AST.statements[1].expression;
is $infix.^name, 'RakuAST::ApplyInfix', 'a bind statement is an ApplyInfix';
is $infix.infix.operator, ':=', 'with a := infix';

# --- EVAL binds -------------------------------------------------------------
is Q|my $a = 1; my $x := $a; $a = 2; $x|.AST.EVAL, 2,
    'a bound scalar follows its source';
is Q|my $x := 5; $x|.AST.EVAL, 5, 'a scalar bound to a literal';
throws-like { Q|my $x := 5; $x = 6|.AST.EVAL }, Exception,
    'a scalar bound to a literal is readonly';
is Q|my @a = 5, 6; my $e := @a[1]; $e = 9; @a|.AST.EVAL, [5, 9],
    'a scalar bound to an element writes through';
is Q|my @src = 1, 2; my @b := @src; @src.push(3); @b|.AST.EVAL, [1, 2, 3],
    'a bound array shares its source';
is Q|my %s = a => 1; my %h := %s; %s<b> = 2; %h.elems|.AST.EVAL, 2,
    'a bound hash shares its source';
is Q|my $y; $y := 3; $y|.AST.EVAL, 3, 'a bind statement binds';
is Q|my $x; ($x := 5) + 1|.AST.EVAL, 6, 'a bind expression yields the bound value';
