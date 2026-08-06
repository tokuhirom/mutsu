use Test;

# `.map(-> $x is rw { ... })` over a concrete `@`-array must rw-alias `$x` to
# each source element's container, same as the `$_`-mutating shape already
# pinned by t/map-native-rw.t. The VM-native `.map` fast path
# (src/vm/vm_native_map.rs) used to pass each element as a plain Value with no
# source name and no ContainerRef cell, so the binder's rw check
# (X::Parameter::RW) rejected the block outright.
# (todo/tickets/map-rw-topic-param-rejected.md)

plan 9;

# --- explicit rw param mutates the source, in place ---
{
    my @a = 1, 2, 3;
    @a.map(-> $x is rw { $x++ });
    is-deeply @a.List, (2, 3, 4), 'an explicit is rw param mutates the source array';
}

# --- the map return value is independent of the writeback ---
{
    my @a = 1, 2, 3;
    my @r = @a.map(-> $x is rw { $x++; $x * 10 });
    is-deeply @r.List, (20, 30, 40), 'the block return value is unaffected by the writeback';
    is-deeply @a.List, (2, 3, 4), 'and the source array is still mutated';
}

# --- is raw behaves the same as is rw for this shape ---
{
    my @a = 1, 2, 3;
    @a.map(-> $x is raw { $x++ });
    is-deeply @a.List, (2, 3, 4), 'an is raw param also mutates the source array';
}

# --- string mutation ---
{
    my @a = <a b c>;
    @a.map(-> $x is rw { $x ~= "!" });
    is-deeply @a.List, ("a!", "b!", "c!"), 'string mutation through an rw param writes back';
}

# --- a plain (non-rw) param is unaffected ---
{
    my @a = 1, 2, 3;
    my @r = @a.map(-> $x { $x + 1 });
    is-deeply @r.List, (2, 3, 4), 'a plain param still returns mapped values';
    is-deeply @a.List, (1, 2, 3), 'and leaves the source array unchanged';
}

# --- typed array keeps its element type across the rw writeback ---
{
    my Int @a = 1, 2, 3;
    @a.map(-> $x is rw { $x++ });
    is @a.WHAT.gist, "(Array[Int])", 'typed array keeps element type after rw-param map';
}

# --- a multi-param block (arity > 1) is unaffected by the rw-param path ---
{
    my @a = 1, 2, 3, 4;
    my @r = @a.map(-> $x, $y { $x + $y });
    is-deeply @r.List, (3, 7), 'a multi-arity block still chunks normally';
}
