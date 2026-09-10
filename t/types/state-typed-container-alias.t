use v6;
use Test;

# ADR-0042 slice 1 §3.1: a `state Int @a` container is boxed into a shared
# `ContainerRef` cell unconditionally (Track B slice 3), but until now
# nothing tagged the ARRAY/HASH inside that cell with its own
# `ContainerTypeInfo` the way a plain `my Int @a` does -- so enforcement
# worked only through the bare declared name `@a` and not through a
# differently-named alias. Companion to t/state-typed-scalar.t (the scalar
# state case) and t/typed-constraint-scope-matrix.t's §3 alias probe (the
# non-state container case).
#
# Expected values verified against raku.

plan 4;

sub direct-array() {
    state Int @a;
    @a.push("bad");
}
dies-ok { direct-array() }, 'state Int @a enforces directly';

sub alias-array() {
    state Int @a;
    my @x := @a;
    @x.push("bad");
}
dies-ok { alias-array() }, 'state Int @a enforces through a differently-named alias';

sub direct-hash() {
    state Int %h;
    %h<k> = "bad";
}
dies-ok { direct-hash() }, 'state Int %h enforces directly';

sub alias-hash() {
    state Int %h;
    my %y := %h;
    %y<k> = "bad";
}
dies-ok { alias-hash() }, 'state Int %h enforces through a differently-named alias';

done-testing;
