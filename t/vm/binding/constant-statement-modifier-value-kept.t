use Test;

plan 9;

# `constant` is bound at compile time and does not respect a runtime
# statement modifier: `constant $w = 11 if COND;` evaluates the initializer
# and binds `$w` unconditionally, regardless of whether COND is True or
# False -- confirmed against real raku (no warning either way). Previously
# `my constant $w = 11 if True;` compiled to a declaration split into an
# always-run `my $w` (init Nil) plus a conditional `$w = 11` *assignment*,
# which silently failed to update the constant, leaving `$w` as `(Any)`.
# See todo/tickets/constant-statement-modifier-value-lost.md.

{
    my constant $w = 11 if True;
    is $w, 11, 'my constant with a True if-modifier keeps its value';
}

{
    my constant $w = 11 if False;
    is $w, 11, 'my constant with a False if-modifier STILL keeps its value (matches raku)';
}

{
    my constant $w = 13 unless False;
    is $w, 13, 'my constant with an unless-modifier keeps its value';
}

{
    my constant $w = 13 unless True;
    is $w, 13, 'my constant with a falsifying unless-modifier still keeps its value';
}

{
    our constant $g = 21 if False;
    is $g, 21, 'our constant with a False if-modifier keeps its value';
}

{
    my constant @a = (1, 2, 3) if False;
    is-deeply @a, (1, 2, 3), 'array constant with a False if-modifier keeps its value';
}

{
    my constant %h = (a => 1, b => 2) if False;
    is %h<a>, 1, 'hash constant with a False if-modifier keeps its value (key a)';
    is %h<b>, 2, 'hash constant with a False if-modifier keeps its value (key b)';
}

# A plain (non-constant) `my` declaration must still be genuinely gated by
# the modifier -- regression guard that the constant-specific bypass above
# does not leak into ordinary variable declarations.
{
    my $x = 5 if False;
    ok !$x.defined, 'plain my var with a False if-modifier stays undefined (not a constant)';
}
