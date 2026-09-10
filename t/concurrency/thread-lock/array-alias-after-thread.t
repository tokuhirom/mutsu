use v6;
use Test;

plan 6;

# The cross-thread array store is keyed by NAME, and `shared_vars_active` never
# goes back to false once a thread has run. Routing every plain-lexical `@a.push`
# through that store therefore detached a frame-local array from every other
# binding of the same container: `my @t := @a` kept the original node while the
# push landed under `__mutsu_atomic_arr::@a`. Only a genuinely-shared name (or a
# worker thread, where serializing concurrent appends is the point) may use it.

sub aliased($tag) {
    my @a;
    my @b;
    my $cond = False;
    # A non-trivial RHS binds by value, so the alias is pure container identity.
    my @t := $cond ?? @b !! @a;
    @a.push("via-a");
    @t.push("via-t");
    @a.join(",")
}

is aliased("before"), "via-a,via-t", 'a ternary-bound array alias shares pushes';

# Spawning (and awaiting) a thread must not change that.
is (await start { 42 }), 42, 'a start block runs';

is aliased("after"), "via-a,via-t",
    'the alias still shares pushes after a thread has run';

# The same for a direct bind, and for an array declared before the thread ran.
{
    my @src;
    my @alias := @src;
    await start { 1 };
    @src.push("x");
    @alias.push("y");
    is @src.join(","), "x,y", 'a direct alias shares pushes after a thread ran';
}

# A genuinely shared array still merges concurrent pushes.
{
    my @shared;
    await Promise.allof((^4).map: -> $i { start { @shared.push($i) } });
    is @shared.elems, 4, 'concurrent pushes to a shared array all land';
    is @shared.sort.join(","), "0,1,2,3", 'and none of them is lost';
}
