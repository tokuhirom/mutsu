use Test;

plan 15;

# A `Code` object inside a list rendered as the EMPTY STRING, so a one-element
# list printed as `()` and read as empty. The pure renderers (`gist_value` /
# `raku_value`) have no Code arm, so a Code element fell through to the `.Str`
# rule -- which for Code is the bare name, empty for an anonymous block. The
# real `Code.gist`/`Code.raku` live in the Sub method handler; the container
# renderers now route Code leaves to it, the way they already did for an
# instance carrying a user-defined `method gist`.
#
# The `#`(Block|N)` address can never match rakudo's, so these assert the
# SHAPE, not the exact string.

my &b = { $^a };

# --- the severe defect: the element must not vanish -------------------------

{
    my $g = (&b,).gist;
    isnt $g, '()', 'a one-element list holding a Block does not render as empty';
    ok $g.starts-with('(-> $a '), 'the list element renders as a Block with its signature';
    ok $g.contains('Block|'), 'the Block element keeps its declarator comment';
}

{
    my $g = [&b].gist;
    isnt $g, '[]', 'a one-element array holding a Block does not render as empty';
    ok $g.starts-with('[-> $a '), 'the array element renders as a Block';
}

{
    my $r = (&b,).raku;
    isnt $r, '(,)', '.raku of a list holding a Block does not drop the element';
    ok $r.starts-with('(-> $a '), '.raku renders the Block element';
    ok $r.ends-with(',)'), '.raku keeps the one-element list comma';
}

# --- a named Sub keeps its & inside a container ------------------------------

{
    sub f($x) { }
    is (&f,).gist, '(&f)', 'a named Sub inside a list keeps its & sigil';
    is &f.gist, '&f', '... and is unchanged outside one';
}

# --- the bare-Block gist uses the -> form, not `sub { }` ---------------------

# `{ $^a }` is a Block in raku, not an anonymous Sub: a placeholder block is
# only a Block spelled with its signature implied.
{
    is &b.^name, 'Block', 'a placeholder block is a Block, not a Sub';
    ok &b.gist.starts-with('-> $a '),
        'a placeholder block gists in the -> form with its signature';
    ok !&b.gist.starts-with('sub '), '... and not as an anonymous sub';
}

# The `^` twigil is declaration syntax, not part of the parameter's name.
{
    ok !&b.gist.contains('$^a'), 'the placeholder twigil is not shown in the signature';
}

# An anonymous `sub` really is a Sub and keeps the `sub` declarator.
{
    my $s = sub ($a) { $a };
    ok $s.raku.starts-with('sub ($a) '), 'an anonymous sub still renders as a Sub';
}
