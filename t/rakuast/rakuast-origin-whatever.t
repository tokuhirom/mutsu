use Test;

# Terms that hold a `*` the whole-unit pass curries still carry the origins the
# parser read them from (ADR-10723 Stage 2), and the curried `*` itself, which
# Rakudo gives no origin, has none. Ranges measured on rakudo 2026.09.

plan 9;

my $src = q:to/END/;
my @a = (1, 2, 3)
  .map(* + 1)
  .grep(* > 2);
say @a.map(*.abs).sum;
END
my $ast = $src.AST;

my %seen;       # "from..to" => node classes that carry that range
my @argument-origins;
sub walk($node) {
    with $node.origin {
        %seen{"{.from}..{.to}"}.push: $node.^name;
    }
    @argument-origins.push: $node.origin.defined
        if $node.^name eq 'RakuAST::WhateverCode::Argument';
    $node.visit-children(&walk);
}
$ast.visit-children(&walk);

my @s = $ast.statements;
sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
is range(@s[0].expression.initializer.expression), '8..46',
    'a method chain over three lines, whatever-curried arguments and all';
ok %seen<25..30>, 'the curried `* + 1` argument';
ok %seen<40..45>, 'the curried `* > 2` argument';
ok %seen<9..10>, 'a literal in the same statement';
is range(@s[1].expression), '48..69', 'a call over a curried method chain';
ok %seen<52..69>, 'the chain it is called on';
ok %seen<59..64>, 'the curried `*.abs`';
is @argument-origins.elems, 3, 'three curried `*`s';
nok @argument-origins.any, 'none of them has an origin';
