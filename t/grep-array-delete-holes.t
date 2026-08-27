use Test;

plan 12;

# `.grep` over an array promotes each matched source slot to a shared
# `ContainerRef` cell so a writeback loop (`@a.grep(...)>>++`) mutates through
# into the source. A `:delete`d slot must be excluded from that promotion: it has
# no element container to alias, and `ArrayData::hole_at` recognises a hole by
# the gap marker value sitting in the slot *plus* its absence from
# `initialized` — so wrapping the marker in a container made the slot read as a
# live element while `initialized` still called it empty, and a later
# trailing-slot `:delete` stopped truncating the array.

sub after-grep(&touch) {
    my @a = <a b c d>;
    @a[2]:delete;
    touch(@a);
    @a[3]:delete;   # deleting the last slot must trim the trailing hole too
    +@a;
}

is after-grep(-> @x { }), 2, 'baseline: trailing delete trims the hole before it';
is after-grep(-> @x { @x.grep({ True }).join(",") }), 2,
    'a grep whose matcher accepts the hole does not materialize it';
is after-grep(-> @x { @x.grep({ True }) }), 2, 'even unconsumed';
is after-grep(-> @x { @x.grep({ $_.defined }).join(",") }), 2,
    'a grep that rejects the hole is unaffected';
is after-grep(-> @x { @x.grep({ False }).join(",") }), 2, 'a grep that matches nothing';
is after-grep(-> @x { @x.grep(/./).join(",") }), 2, 'a regex matcher';
is after-grep(-> @x { @x.map({ $_ }).join(",") }), 2, 'the .map sibling stays correct';

# The grep result still reports the hole as an undefined element.
{
    my @a = <a b c d>;
    @a[2]:delete;
    is-deeply @a.grep({ True }).map({ $_ // 'H' }).List, ('a', 'b', 'H', 'd'),
        'the hole appears in the grep result as an undefined value';
}

# The writeback aliasing the promotion exists for must still work.
{
    my @a = 1, 2, 3, 4;
    @a.grep(* %% 2)>>++;
    is-deeply @a.List, (1, 3, 3, 5), 'hyper ++ through a grep result writes back';
}
{
    my @a = 1, 2, 3, 4;
    for @a.grep(* %% 2) { $_++ }
    is-deeply @a.List, (1, 3, 3, 5), 'a for loop over a grep result writes back';
}
{
    my @a = 1, 2, 3, 4;
    my @g = @a.grep(* %% 2);
    @g>>++;
    is-deeply @a.List, (1, 2, 3, 4), 'a named copy of a grep result does not write back';
}
# ...including when a hole is present but not matched.
{
    my @a = 1, 2, 3, 4, 5;
    @a[1]:delete;
    for @a.grep({ $_.defined }) { $_ += 100 }
    is-deeply @a.map({ $_ // 'H' }).List, (101, 'H', 103, 104, 105),
        'writeback still works with an unmatched hole in the source';
}
