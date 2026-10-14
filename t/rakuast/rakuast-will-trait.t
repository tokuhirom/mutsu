use Test;

# `will PHASE { ... }` on a variable declaration in RakuAST, measured on rakudo
# 2026.09: the declaration's `traits` hold a `Trait::Will(phase => "begin",
# block => Block)` per trait, and EVAL of the tree runs the block in its phase
# with `$_` bound to the declared container.

plan 10;

sub decl($src) { $src.AST.statements[0].expression }

# --- the tree
{
    my $d = decl(Q[my $x will begin { $_ = 42 }]);
    isa-ok $d, RakuAST::VarDeclaration::Simple, '`my $x will begin {...}` is a VarDeclaration::Simple';
    is $d.traits.elems, 1, '... with one trait';
    isa-ok $d.traits[0], RakuAST::Trait::Will, '... a Trait::Will';
    is $d.traits[0].phase, 'begin', '... of the `begin` phase';
    isa-ok $d.traits[0].block, RakuAST::Block, '... over the written block';
}
{
    my $d = decl(Q[my $y will leave { 1 } will enter { 2 } = 3]);
    is $d.traits.map(*.phase).join(','), 'leave,enter', 'several `will` traits keep their order';
    isa-ok $d.initializer, RakuAST::Initializer::Assign, '... and the initializer follows';
}

# --- EVAL of the tree
is Q[my $x will begin { $_ = 3 }; $x].AST.EVAL, 3, '`will begin` assigns to the container';
is-deeply Q[my @a will begin { .push(1, 2) }; @a].AST.EVAL, [1, 2], '... and pushes onto an array';
is Q[my $seen; my $w will begin { $seen = .defined }; $seen].AST.EVAL, False,
    'the container is in its static state at BEGIN time';
