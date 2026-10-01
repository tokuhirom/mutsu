use Test;

# A block's own placeholder scope reaches every expression and statement
# header position, not only the handful a hand-rolled walker listed: the
# placeholder collector now walks the typed AST visitor (ADR-0137). Each
# assertion was checked against rakudo.

plan 14;

# --- positions that make the placeholder the block's parameter ------------
{
    my &f = { my $s = 0; loop (my $i = 0; $i < $^n; $i++) { $s++ }; $s };
    is &f.arity, 1, 'a placeholder in a C-style loop header is the block\'s';
    is f(3), 3, '... and it is bound';
}
{
    my &f = { my @r; $^a ==> @r; @r };
    is &f.arity, 1, 'a placeholder fed by ==> is the block\'s';
    is-deeply f(5), [5], '... and it is bound';
}
{
    my &f = {
        my $s = qq:to/END/;
            v=$^a
            END
        $s
    };
    is &f.arity, 1, 'a placeholder in an interpolating heredoc is the block\'s';
    is f(5).trim, 'v=5', '... and it is bound';
}
{
    my &f = { my $x where $^a > 0 = 1; $x };
    is &f.arity, 1, 'a placeholder in a variable\'s where thunk is the block\'s';
}
{
    my &f = { ($^x = 5) };
    is &f.arity, 1, 'an assigned placeholder in expression position is the block\'s';
}

# --- source order of a statement modifier ----------------------------------
{
    my &f = { $^b if $b };
    is f(3), 3, 'a statement modifier\'s statement precedes its condition';
    my $r;
    my &g = { $r = $^b for $b };
    g(4);
    is $r, 4, '... for a for-modifier too';
}

sub class-of($code) { (try EVAL $code); $! ?? $!.^name !! 'no error' }
is class-of('my &f = { $b = 1 if $^b }'), 'X::Undeclared',
    'a bare use in a modifier statement precedes the placeholder in its condition';
is class-of('my &f = { say $b if 1; say $^b }'), 'X::Undeclared',
    'a bare use in an earlier if-modifier statement precedes the placeholder';

# --- the mainline rejects placeholders in the same positions ---------------
is class-of('say $^a if True'), 'X::Placeholder::Mainline',
    'a placeholder in a mainline if-modifier is the mainline\'s';
is class-of('say * + $^a'), 'X::Placeholder::Mainline',
    'a placeholder inside a WhateverCode at the mainline is the mainline\'s';
