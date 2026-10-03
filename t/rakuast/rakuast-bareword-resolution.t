use v6;
use experimental :rakuast;
use Test;

# What a bareword renders as across the RakuAST boundary. Rakudo resolves the
# name at parse time, so the node says what it is (measured on rakudo 2026.09):
# a type is a `Type::Simple`, a setting enum value a `Term::Enum`, any other
# defined term (a setting constant, the unit's enum value, a sigilless
# parameter) a `Term::Name`, and an argument-less redispatch call a
# `Call::Name::WithoutParentheses`. Each EVALs back to what the name means.

plan 28;

sub expr(Str $src, Int $i = 0) { $src.AST.statements[$i].expression }

# --- a name the unit declares, seen from inside a block ---------------------
is expr(Q|class C { }; { C }|, 1).body.statement-list.statements[0].expression.^name,
    'RakuAST::Type::Simple', 'a class name inside a block is a Type::Simple';
is Q|class D { method v { 7 } }; (1, 2).map({ D.new.v }).sum|.AST.EVAL, 14,
    'and a closure that names it EVALs';
is Q|constant K = 3; sub f { { K } }; f()|.AST.EVAL, 3,
    'a constant named inside a nested block';

# --- setting terms and enum values ------------------------------------------
is expr(Q|IterationEnd|).^name, 'RakuAST::Term::Name', 'IterationEnd is a Term::Name';
is expr(Q|Order::Less|).^name, 'RakuAST::Term::Name', 'a qualified enum value is a Term::Name';
is expr(Q|Less|).^name, 'RakuAST::Term::Enum', 'a bare setting enum value is a Term::Enum';
is expr(Q|Kept|).^name, 'RakuAST::Term::Enum', 'PromiseStatus::Kept, bare';
ok Q|IterationEnd|.AST.EVAL =:= IterationEnd, 'IterationEnd EVALs to the sentinel';
is Q|Less|.AST.EVAL, Less, 'Less EVALs to Order::Less';
is Q|Order::More|.AST.EVAL, More, 'Order::More EVALs';
is Q|1 cmp 2 == Less|.AST.EVAL, True, 'an enum value in an expression';
is Q|Kept|.AST.EVAL, PromiseStatus::Kept, 'Kept EVALs';
is expr(Q|Supply|).^name, 'RakuAST::Type::Simple', 'a setting type stays a Type::Simple';

# --- the unit's own enum values ---------------------------------------------
is expr(Q|enum Colour <Red Green>; Red|, 1).^name, 'RakuAST::Term::Name',
    "the unit's enum value is a Term::Name";
is expr(Q|enum Hue <Cyan Magenta>; Hue::Magenta|, 1).^name, 'RakuAST::Term::Name',
    'and so is its qualified spelling';
is Q|enum Tint <Dark Light>; Light.value|.AST.EVAL, 1, 'an enum value EVALs';
is Q|enum Shade <Grey Black>; Shade::Grey.key|.AST.EVAL, 'Grey', 'a qualified enum value EVALs';

# --- sigilless parameters ---------------------------------------------------
my $sub = expr(Q|sub f(\x) { x }|);
is $sub.signature.parameters[0].target.^name, 'RakuAST::ParameterTarget::Term',
    'a sigilless parameter targets a term';
is $sub.signature.parameters[0].target.name.raku, 'RakuAST::Name.from-identifier("x")', 'named after the parameter';
is $sub.body.statement-list.statements[0].expression.^name, 'RakuAST::Term::Name',
    'and its use is a Term::Name';
is Q|sub f(\x) { x * 2 }; f(21)|.AST.EVAL, 42, 'a sigilless sub parameter EVALs';
is Q|my @a = 1, 2; sub g(\l) { l.push(3) }; g(@a); @a.elems|.AST.EVAL, 3,
    'a sigilless parameter binds the argument itself';
is expr(Q|-> \v { v }|).signature.parameters[0].target.^name, 'RakuAST::ParameterTarget::Term',
    'a sigilless pointy-block parameter targets a term';
is Q|my &g = -> \v { v + 1 }; g(1)|.AST.EVAL, 2, 'a sigilless pointy-block parameter EVALs';

# --- redispatch calls without arguments -------------------------------------
my $call = expr(Q|sub f { callsame }|).body.statement-list.statements[0].expression;
is $call.^name, 'RakuAST::Call::Name::WithoutParentheses', 'callsame is a call';
is $call.name.raku, 'RakuAST::Name.from-identifier("callsame")', 'named callsame';
is Q|class A { method m { "A" } }; class B is A { method m { "B" ~ callsame } }; B.new.m|.AST.EVAL,
    'BA', 'callsame EVALs as a redispatch';
is Q|multi f(Int $x) { nextsame }; multi f(Any $x) { "any" }; f(1)|.AST.EVAL, 'any',
    'nextsame EVALs as a redispatch';
