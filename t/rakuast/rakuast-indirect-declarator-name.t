use v6;
use Test;
use experimental :rakuast;

# `sub ::(EXPR)`, `method ::(EXPR)` and `class ::(EXPR)` name the declaration
# with a computed name: `Name(Part::Empty, Part::Expression(EXPR))`.
plan 10;

for <sub method class> -> $kind {
    my $src = $kind eq 'sub'    ?? 'sub ::("x") { 1 }'
           !! $kind eq 'method' ?? 'class D { method ::("x") { 1 } }'
           !!                      'class ::("X") { }';
    my $ast = $src.AST;
    my $decl = $kind eq 'method'
        ?? $ast.statements[0].expression.body.body.statement-list.statements[0].expression
        !! $ast.statements[0].expression;
    my @parts = $decl.name.parts;
    is @parts.elems, 2, "$kind: the name has an empty edge and an expression";
    isa-ok @parts[1], RakuAST::Name::Part::Expression, "$kind: the second part is an expression";
}

is EVAL('sub ::("rk_indirect_sub") ($x) { $x + 1 }; rk_indirect_sub(41)'.AST), 42,
    'a literal-named sub round-trips and runs';
is EVAL('class ::("RkIndirectClass") { method m { 7 } }; RkIndirectClass.m'.AST), 7,
    'a literal-named class round-trips and runs';
is EVAL('class E { method ::("sp ace") { 23 } }; E.new."sp ace"()'.AST), 23,
    'a literal-named method round-trips and runs';
isa-ok EVAL('anon sub :: ($x) { $x + 1 }'.AST), Sub, 'a nameless sub is still anonymous';
