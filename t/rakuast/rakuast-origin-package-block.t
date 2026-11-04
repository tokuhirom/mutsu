use Test;

# A `package` or `module` declaration has the ranges of its name and block
# (ADR-10723 Stage 2, #15780), as a `class` already did: the `Name`, the
# `Block`, its `Blockoid` and `StatementList`. Ranges measured on rakudo
# 2026.09.

plan 3;

sub ranges(Str $src, *@classes) {
    my @r;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        if $name eq @classes.any {
            with $node.origin { @r.push: "$name {.from}-{.to}" }
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.sort.join(', ')
}

is ranges('package Pkg { say 1 }', 'Block', 'Blockoid', 'StatementList', 'Name'),
    'Block 12-21, Blockoid 12-21, Name 14-17, Name 8-11, StatementList 14-19',
    'a package';
is ranges('module Mod { say 1 }', 'Block', 'Blockoid', 'StatementList', 'Name'),
    'Block 11-20, Blockoid 11-20, Name 13-16, Name 7-10, StatementList 13-18',
    'a module';
is ranges('my package P { my $x = 1 }', 'Block', 'Name'),
    'Block 13-26, Name 11-12',
    'a lexical package';
