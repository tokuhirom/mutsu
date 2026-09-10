use v6;
use Test;

# `list` and `cache` used as bare listops with NO argument must parse as a
# zero-arg call yielding the empty list, matching raku. Regression: mutsu's
# `list`/`cache` special case unconditionally parsed a mandatory comma
# expression as the argument, so `list;` (or `list` at statement end) aborted
# the whole statement with "Confused. expected ... '.' or digits or ...".
# Found in the wild in REPL (lizmat): a bare `list;` call to a `my sub list`.

plan 6;

# Bare builtin `list` with no argument is the empty list.
{
    my @a = list;
    is @a.elems, 0, 'bare `list` with no argument is the empty list';
}

# `list` still takes a comma expression as its argument.
{
    is (list 1, 2, 3).raku, '(1, 2, 3)', '`list 1, 2, 3` still collects its args';
}

# `list;` as a statement calls a user-declared `sub list` (name is not stolen).
{
    my $ran = 0;
    my sub list() { $ran = 42 }
    list;
    is $ran, 42, '`list;` dispatches to a user-declared sub list';
}

# `list` at end of a block body (no trailing semicolon) parses too.
{
    my $r = do { list };
    is $r.elems, 0, '`list` as the last statement of a block parses';
}

# `cache` with no argument parses (zero-arg call).
{
    my $c = cache;
    ok $c.defined || !$c.defined, '`cache` with no argument parses';
}

# `list` followed by a statement modifier is a zero-arg call, not an error.
{
    my @a = (list if True);
    is @a.elems, 0, '`list if COND` parses as a zero-arg call under the modifier';
}
