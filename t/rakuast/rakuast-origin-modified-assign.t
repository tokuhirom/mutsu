use Test;

# A statement-modified assignment (`$n = 5 if $n`): the assigned variable is
# built without taking the range of the condition's equal `$n`, so the
# condition keeps its own (ADR-10723 Stage 2, #15780). Ranges measured on
# rakudo 2026.09.

plan 2;

sub ranges(Str $src, *@classes) {
    my @r;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        if $name eq @classes.any {
            with $node.origin { @r.push: "$name {.from}..{.to}" }
        }
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r.sort.join(', ')
}

is ranges('my $n = 1; $n = 5 if $n;', 'Var::Lexical', 'Assignment'),
    'Assignment 14..15, Var::Lexical 11..13, Var::Lexical 21..23',
    'the target and the condition each have a range';
is ranges('my $n = 1; $n = 5 if $n.^name eq "x";', 'Var::Lexical', 'Call::MetaMethod'),
    'Call::MetaMethod 25..29, Var::Lexical 11..13, Var::Lexical 21..23',
    'a call in the condition';
