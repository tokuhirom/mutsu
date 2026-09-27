use v6;
use Test;

# A frame can hold several slots named `&f` (an inner block's `my &f`, a nested
# `if EXPR -> &f`). A `&f` read used to probe the frame's slots by name at run
# time and always found the first one, so an inner read saw the outer binding.
# Found through Identity::Utils' EXPORT (`if ... -> &code { } else { if ... ->
# &code { &code } }`). A pointy `if` parameter also leaked into the enclosing
# scope, overwriting a same-named outer variable.

plan 12;

{
    my &f = &uc;
    {
        my &f = &lc;
        is &f('AbC'), 'abc', 'inner my &f is read inside its block';
        is &f.name, 'lc', 'inner my &f as a value';
    }
    is &f.name, 'uc', 'outer my &f is read after the block';
}

sub nested-else() {
    if 0 -> &c { 'outer' }
    else {
        if &uc -> &c { &c.name }
    }
}
is nested-else(), 'uc', 'nested -> &c in an else branch sees its own binding';

sub nested-then() {
    if &lc -> &c {
        if &uc -> &c { &c.name }
    }
}
is nested-then(), 'uc', 'nested -> &c in a then branch sees its own binding';

sub param-shadow(&c) {
    my @seen;
    if 1 -> &c { @seen.push: &c.^name }
    @seen.push: &c.name;
    @seen
}
is-deeply param-shadow(&uc), ['Int', 'uc'], 'pointy &c shadows a &c parameter only inside the if';

{
    my $c = 7;
    if 8 -> $c { is $c, 8, 'pointy $c inside the branch' }
    is $c, 7, 'pointy $c does not overwrite the outer $c';
}

sub scalar-param($d) {
    if 9 -> $d { }
    $d
}
is scalar-param(1), 1, 'pointy $d does not overwrite a $d parameter';

{
    my $c = 1;
    if 7 -> $c {
        if 8 -> $c { }
        is $c, 7, 'inner pointy $c does not overwrite the outer pointy $c';
    }
}

{
    my $x = 3;
    is (do if 4 -> $x { $x * 10 }), 40, 'value-position pointy if binds its own $x';
    is $x, 3, 'value-position pointy if leaves the outer $x alone';
}
