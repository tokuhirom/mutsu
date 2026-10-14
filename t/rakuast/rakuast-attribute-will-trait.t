use Test;

# `has $.v will lazy { ... }` in RakuAST, measured on rakudo 2026.09: the
# attribute's `VarDeclaration::Simple` holds a `Trait::Will(phase => "lazy",
# block => Block)`, and EVAL of the tree dispatches `trait_mod:<will>` with the
# block as its positional Callable and the phase as the named marker.

plan 8;

my @seen;
multi sub trait_mod:<will>(Attribute:D $attr, Callable $block, :$lazy!) {
    @seen.push: $attr.name => $block();
}

sub decl($src) {
    $src.AST.statements[0].expression.body.body.statement-list.statements[0].expression
}

{
    my $d = decl(Q[class A { has $.v will lazy { 42 } }]);
    isa-ok $d, RakuAST::VarDeclaration::Simple, 'the attribute is a VarDeclaration::Simple';
    is $d.traits.elems, 1, '... with one trait';
    isa-ok $d.traits[0], RakuAST::Trait::Will, '... a Trait::Will';
    is $d.traits[0].phase, 'lazy', '... of the `lazy` phase';
    isa-ok $d.traits[0].block, RakuAST::Block, '... over the written block';
}

{
    my $ast = Q[class B { has $.w will lazy { 7 } }].AST;
    $ast.EVAL;
    # rakudo also applies the trait while building the tree, so only the
    # last application is pinned.
    ok @seen.elems >= 1, 'the trait was applied';
    is @seen.tail.key, '$!w', '... to the attribute';
    is @seen.tail.value, 7, '... passing the written block';
}
