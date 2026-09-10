use Test;

# `.map(-> $x is rw { ... })` over a concrete `@`-array must rw-alias `$x` to
# each source element's container, same as the `$_`-mutating shape already
# pinned by t/map-native-rw.t. The VM-native `.map` fast path
# (src/vm/vm_native_map.rs) used to pass each element as a plain Value with no
# source name and no ContainerRef cell, so the binder's rw check
# (X::Parameter::RW) rejected the block outright.
# (todo/tickets/map-rw-topic-param-rejected.md)
#
# A typed/constrained param (`-> Int $x is rw { }`) and a body containing
# `next`/`last` both defer past the VM-native fast path to the interpreter's
# own map orchestration (src/runtime/resolution_map_grep_rw.rs), which needed
# the same ContainerRef-cell promotion separately -- it silently dropped the
# writeback instead of raising or mutating correctly.
# (todo/tickets/map-rw-param-interpreter-fallback-still-silent.md)

plan 14;

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

# --- a typed rw param defers past the VM-native fast path but still writes back ---
{
    my @a = 1, 2, 3;
    @a.map(-> Int $x is rw { $x++ });
    is-deeply @a.List, (2, 3, 4), 'a typed is rw param still mutates the source array';
}

# --- next inside an rw-param block skips only its own mutation ---
{
    my @a = 1, 2, 3, 4, 5;
    my @r = @a.map(-> $x is rw { next if $x %% 2; $x++; $x });
    is-deeply @a.List, (2, 2, 4, 4, 6), 'next inside an rw-param block skips its own mutation';
    is-deeply @r.List, (2, 4, 6), 'and the return value omits the skipped iterations';
}

# --- last inside an rw-param block stops the loop but keeps prior writebacks ---
{
    my @a = 1, 2, 3, 4, 5;
    my @r = @a.map(-> $x is rw { last if $x == 4; $x++; $x });
    is-deeply @a.List, (2, 3, 4, 4, 5), 'last inside an rw-param block stops after its own mutation';
    is-deeply @r.List, (2, 3, 4), 'and the return value stops at the same point';
}
