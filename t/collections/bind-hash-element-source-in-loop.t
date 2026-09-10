use Test;

# `%h{$k} := %d<elem>` — binding a hash/array element TARGET to an indexed
# SOURCE inside a loop. The RHS index compiled through the `is rw` call-arg
# writeback temps (`__mutsu_index_rw_arg_N`), which are compile-time-fixed
# globals reused verbatim on every loop iteration; their "write through the
# existing ContainerRef" semantics made the first key's bound element track the
# LATEST source value instead of the one it was bound to. (Template::Mustache's
# `load-specs` does `%specs{.basename} := %data<tests>` in a loop, so the first
# spec file got the LAST file's tests and a plan count of 0.)

plan 4;

my %s;
for 1..5 -> $k {
    my %d = t => [$k * 10];
    %s{$k} := %d<t>;
}
is-deeply %s{1}<>, [10], "first bound element keeps its own source (hash source)";
is-deeply %s{5}<>, [50], "last bound element is correct too";
is %s.elems, 5, "all keys distinct";

# Array-element target, array-element source, loop.
my @a;
for 0..3 -> $i {
    my @d = $i * 100, $i * 100 + 1;
    @a[$i] := @d[0];
}
is-deeply @a[0..3].map(*.self), (0, 100, 200, 300),
    "first array element keeps its own source";
