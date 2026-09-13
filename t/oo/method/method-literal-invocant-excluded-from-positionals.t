use Test;

# Companion to `method-literal-is-a-routine.t`, which pins what each declarator
# REPORTS. This pins the other half of tokuhirom/mutsu#8311: what the invocant
# does to the signature.
#
# A method literal's declared invocant is consumed by the `Invocant:` marker and
# never enters the positional list. While `anon method (...)` was routed through
# the anonymous-SUB parser it had no invocant at all, so the receiver was slurped
# as an ordinary positional:
#
#   $ raku  -e 'class W {}; sub c(&r) { r(W, 1, 2) }; say c(anon method (W: *@a) { @a.join(",") })'
#   1,2
#   $ mutsu -e ' ... same ... '
#   ,1,2
#
# (`W.join` stringifies to empty, hence the leading comma.) A slurpy is what
# makes this visible -- a fixed-arity signature would have failed loudly on the
# arity instead of quietly binding one argument too many.
#
# The `::` spelling is the shape #7954 found in the wild: `UI::HTMLWindow` calls
# `&routine.wrap(anon method :: (Window: *@_, *%_) { ... })`.

plan 10;

class W { }
sub call-it(&r, |c) { r(|c) }

# --- a type-object invocant marker in front of a POSITIONAL slurpy ---------

is call-it(anon method (W: *@a) { @a.join(',') }, W, 1, 2), '1,2',
    'the declared invocant is consumed, not slurped into *@a';
is call-it(method (W: *@a) { @a.join(',') }, W, 1, 2), '1,2',
    '...for the bare `method` spelling too';
is call-it(submethod (W: *@a) { @a.join(',') }, W, 1, 2), '1,2',
    '...and for `submethod`';
is call-it(anon method :: (W: *@a) { @a.join(',') }, W, 1, 2), '1,2',
    '...and with the `::` null declarator name';

# An invocant in front of a NAMED slurpy leaves the positionals alone too.
is call-it(anon method (W: *@a, *%h) { "{@a.join(',')}/{%h<k> // 'none'}" }, W, 1, :k<v>),
    '1/v',
    'a named slurpy beside a positional one still splits correctly';

# --- and in front of ordinary positionals ---------------------------------

is call-it(anon method (W: $x) { $x * 2 }, W, 21), 42,
    'an ordinary positional after the invocant binds the SECOND argument';
is call-it(anon method (W: $x, $y) { $x - $y }, W, 10, 4), 6,
    '...and two of them keep their order';

# `self` is the invocant, not the first positional.
is call-it(anon method (W: $x) { self === W && $x == 1 }, W, 1), True,
    'self is the receiver while $x is the first positional';

# --- an invocant is not required ------------------------------------------

is call-it(anon method (*@a) { @a.join(',') }, W, 1, 2), '1,2',
    'an undeclared invocant is still consumed by the implicit one';

# --- the neighbouring shape that must NOT do this -------------------------

# A `sub` has no invocant concept, so its slurpy takes everything. This is the
# behaviour the method literals wrongly had.
is call-it(sub (*@a) { @a.elems }, W, 1, 2), 3,
    'a sub literal still slurps all three arguments';
