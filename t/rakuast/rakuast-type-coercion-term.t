use v6;
use Test;

plan 10;

# `Int()` / `Int(Str)` / `K()` in term position is a `Type::Coercion`, in both
# directions (ADR-15760 slice 2; found by the EVAL($ast) triage of #16356).

use MONKEY-SEE-NO-EVAL;

sub body-of($ast) { $ast.statements[0].expression }

my $int-str = Q[Int(Str)].AST;
is body-of($int-str).^name, 'RakuAST::Type::Coercion', 'Int(Str) is a coercion type';
is body-of($int-str).base-type.^name, 'RakuAST::Type::Simple', 'its base-type';
is body-of($int-str).constraint.^name, 'RakuAST::Type::Simple', 'its constraint';
is body-of(Q[Int()].AST).^name, 'RakuAST::Type::Coercion', 'Int() is a coercion type';
is body-of(Q[Int("5")].AST).^name, 'RakuAST::ApplyPostfix', 'a call with a value stays a call';

is EVAL(Q[Int(Str).^name].AST), 'Int(Str)', 'EVAL of the round-tripped Int(Str)';
is EVAL(Q[class K {}; K().^name].AST), 'K(Any)', 'a declared class coerces with ()';
is EVAL(Q[Int("5").raku].AST), '5', 'Int("5") still coerces the value';

my $built = RakuAST::StatementList.new(RakuAST::Statement::Expression.new(
    expression => RakuAST::Type::Coercion.new(
        base-type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("Int")))));
is EVAL($built).^name, 'Int(Any)', 'a hand-built Type::Coercion evaluates';

is EVAL(Q[{ my module foo {}; sub foo() { "OH HAI" }; foo().^name ~ &foo() }].AST),
    'foo(Any)OH HAI', 'a type shadowed by a sub keeps the coercion type for foo()';
