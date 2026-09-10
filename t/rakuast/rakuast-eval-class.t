use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST class / role / method / attribute lowering (ADR-0011, the write
# direction).
#
# `RakuAST::Class`, `RakuAST::Role`, `RakuAST::Method`, and the
# `VarDeclaration::Simple` with `scope => "has"` that an attribute renders as
# have been readable since Phase 2 slice 13, but none of them lowered: `EVAL`
# refused the whole tree. They now lower to `Stmt::ClassDecl` /
# `Stmt::RoleDecl` / `Stmt::MethodDecl` / `Stmt::HasDecl` and run through the
# ordinary compiler and VM.
#
# A class declaration is the last statement of each EVAL'd program, so the
# EVAL'd value is the type object and the test calls into it from the outside.
# (Referring to the class *inside* the same program by bare name is a separate,
# still-open read-direction gap: a user type name renders as a bareword.)
#
# Each test uses a distinct class name because `.AST` registers the symbol and
# raku rejects redeclaration.
#
# Passes under BOTH mutsu and raku.

plan 15;

# --- an empty class ---------------------------------------------------------
is EVAL(Q[class E1 { }].AST).^name, 'E1',
    'an empty class lowers to a type object with its name';

# --- a class with a method --------------------------------------------------
my $c2 = EVAL(Q[class E2 { method m() { 42 } }].AST);
is $c2.^name, 'E2', 'a class with a method keeps its name';
is $c2.m, 42, 'the lowered method runs';

# --- a method with parameters -----------------------------------------------
is EVAL(Q[class E3 { method add($a, $b) { $a + $b } }].AST).add(3, 4), 7,
    'a lowered method takes its parameters';

# --- a public attribute -----------------------------------------------------
my $c4 = EVAL(Q[class E4 { has $.x; method double() { $!x * 2 } }].AST);
is $c4.new(x => 21).double, 42, 'a lowered public attribute is set by .new';
is $c4.new(x => 5).x, 5, 'a lowered public attribute has its accessor';

# --- a private attribute ----------------------------------------------------
my $c5 = EVAL(Q[class E5 { has $!n; method m() { 7 } }].AST);
is $c5.m, 7, 'a class with a private attribute lowers';
nok $c5.^can('n'), 'a private attribute gets no public accessor';

# --- a typed attribute ------------------------------------------------------
is EVAL(Q[class E6 { has Int $.n; method plus1() { $!n + 1 } }].AST).new(n => 4).plus1, 5,
    'a lowered typed attribute keeps its type constraint';

# --- return types in all three spellings ------------------------------------
is EVAL(Q[class E7 { method m(--> Int) { 9 } }].AST).m, 9,
    'a `-->` return type lowers on a method';
is EVAL(Q[class E8 { method m() returns Int { 10 } }].AST).m, 10,
    'a `returns` trait lowers on a method';

# --- the lowered return type is still enforced ------------------------------
throws-like { EVAL(Q[class E9 { method m(--> Int) { "nope" } }].AST).m }, Exception,
    'the lowered method return type is checked at run time';

# --- a role lowers the same way (its body is a RoleBody, not a Block) -------
my $r1 = EVAL(Q[role E10 { method m() { 5 } }].AST);
is $r1.^name, 'E10', 'a role lowers to a role type object with its name';
is $r1.m, 5, 'the lowered role method runs on the punned role';

is EVAL(Q[role E11 { }].AST).^name, 'E11', 'an empty role lowers';
