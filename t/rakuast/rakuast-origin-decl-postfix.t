use Test;

# A bare declaration used as a postfix operand (`my $k.defined`) has the
# range of `my $k`, and the call after it has its own (ADR-10723 Stage 2,
# #15780). Ranges measured on rakudo 2026.09.

plan 3;

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

is ranges('my $k.defined;', 'VarDeclaration::Simple', 'Call::Method', 'ArgList'),
    'ArgList 13..13, Call::Method 6..13, VarDeclaration::Simple 0..5',
    'a scalar declaration and its method call';
is ranges('my $k.defined; (my @a.elems, 1);', 'VarDeclaration::Simple'),
    'VarDeclaration::Simple 0..5, VarDeclaration::Simple 16..21',
    'a declaration inside a list';
is ranges('my $k.defined;', 'Name'), 'Name 6..13', 'the method name';
