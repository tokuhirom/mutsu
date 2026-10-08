use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# A bareword the unit declares renders as what it declares (rakudo resolves
# it at parse time). Expected shapes measured on Rakudo 2026.09.

plan 8;

# --- a `&cb` parameter: a bare `cb` is an argument-less call ----------------

{
    my $ast = 'sub f(&cb) { cb }'.AST;
    my $call = $ast.statements[0].expression.body.statement-list.statements[0].expression;
    isa-ok $call, RakuAST::Call::Name::WithoutParentheses, 'a bare `cb` is a call without parentheses';
    is $call.name.canonicalize, 'cb', 'it names the parameter';
}

# --- `my (\a, \b) := ...` declares terms ------------------------------------

{
    my $ast = 'my (\y1, \y2) := (1, 2); y1 + y2'.AST;
    my $use = $ast.statements[1].expression;
    isa-ok $use.left, RakuAST::Term::Name, 'a destructured sigilless name is a term';
    is EVAL($ast), 3, 'the round trip computes the sum';
}

# --- `package GLOBAL::X::Y { class C }` declares X::Y::C --------------------

{
    my $ast = 'package GLOBAL::X::Demo9 { class C9 { } }; X::Demo9::C9.new.^name'.AST;
    is EVAL($ast), 'X::Demo9::C9', 'a GLOBAL-prefixed package declares the absolute name';
}

# --- an EVAL string sees the enum values and lexical classes of its caller ---

{
    enum E9 <aa bb>;
    is EVAL('aa'), E9::aa, 'a bare enum value resolves in EVAL';
    is EVAL('E9::bb'), E9::bb, 'a qualified enum value resolves in EVAL';
    my class Lex9 { has $.x = 16 }
    my $Lex9 = 5;
    is EVAL('Lex9.new.x'), 16, 'a lexical class resolves in EVAL past a same-named scalar';
}
