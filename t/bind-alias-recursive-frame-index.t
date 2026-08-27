use Test;
plan 10;

# Regression pins for the `:=` ancestor-frame propagation gate
# (`Interpreter::propagate_bind_to_ancestor_frames`,
# `src/vm/vm_var_assign_ops.rs`).
#
# A `:=` bind promotes its source to a shared `ContainerRef` cell and splices
# that cell into the ancestor call frame that declares the source, so the
# binding survives that frame's env restore on return. The frame was found by
# NAME alone, which cannot tell "the one true declaring scope of a captured
# free variable" from "an unrelated ancestor invocation of the same recursive
# routine that legitimately declared its own independent same-named local".
# Under recursion the loop therefore spliced ONE cell into EVERY level, so a
# `$x := $v; $x = 999` performed at the base case reported 999 at every
# recursion depth instead of only the base case's own `$v`.
#
# The fix gates the splice on frame ownership rather than the bare name:
# nothing is spliced when the current invocation declares the source itself,
# and otherwise only the INNERMOST ancestor frame that declares it is patched
# (the tier the env chain's own lookup resolves to). Every assertion below is
# raku's answer, verified with `raku` on the same source.
#
# NB: each subtest's sub/array live at the file's mainline scope with distinct
# names, and the whole-container (`@`) subtest reads `@v[0]` right after the
# if/else on every call. Both are load-bearing to avoid a separate,
# pre-existing, unrelated stack overflow: a recursive sub that builds a
# trailing-comma list literal of its own parameter into a `my @` local
# (`my @v = ($n,);`) and then never reads that local again before the
# recursive call returns crashes with a native Rust stack overflow,
# independent of `:=` bind or block scoping entirely. See
# todo/deep/recursive-sub-trailing-comma-array-literal-of-own-param-stack-overflow.md.

my @rec-scalar-levels;
sub rec-scalar(Int $n) {
    my $v = $n;
    if $n > 0 {
        rec-scalar($n - 1);
    } else {
        my $x := $v;
        $x = 999;
    }
    @rec-scalar-levels.push($v);
}
rec-scalar(3);
is @rec-scalar-levels, (999, 1, 2, 3),
    'recursive scalar := bind at the base case touches only its own level';

my @rec-array-levels;
sub rec-array(Int $n) {
    my @v = ($n,);
    if $n > 0 {
        rec-array($n - 1);
    } else {
        my @x := @v;
        @x[0] = 999;
    }
    @rec-array-levels.push(@v[0]);
}
rec-array(3);
is @rec-array-levels, (999, 1, 2, 3),
    'recursive whole-container @ := bind at the base case touches only its own level';

# Mutual recursion between two subs that each declare the same local name:
# the ancestor chain alternates between two different compiled routines, so
# the by-name match hit frames belonging to a *different* sub as well.
my @mutual-levels;
sub mutual-odd(Int $n) {
    my $v = $n;
    if $n > 0 { mutual-even($n - 1) } else { my $x := $v; $x = 999 }
    @mutual-levels.push($v);
}
sub mutual-even(Int $n) {
    my $v = $n;
    if $n > 0 { mutual-odd($n - 1) } else { my $x := $v; $x = 999 }
    @mutual-levels.push($v);
}
mutual-odd(3);
is @mutual-levels, (999, 1, 2, 3),
    'mutual recursion with a shared local name: only the binding level is affected';

# A recursive METHOD: the frames are method invocations rather than sub calls.
class RecBinder {
    has @.seen;
    method rec(Int $n) {
        my $v = $n;
        if $n > 0 { self.rec($n - 1) } else { my $x := $v; $x = 999 }
        @!seen.push($v);
    }
}
my $binder = RecBinder.new;
$binder.rec(3);
is $binder.seen, (999, 1, 2, 3),
    'recursive method := bind at the base case touches only its own level';

# The case the propagation mechanism exists for, crossed with recursion: the
# bind's source is a genuine free variable declared several frames up, so the
# splice MUST still reach that one declaring frame (and no recursion level's
# own same-named local may be disturbed).
my $free-outer = 1;
my $free-alias;
my @free-levels;
sub rec-free(Int $n) {
    my $v = $n;
    if $n > 0 { rec-free($n - 1) } else { $free-alias := $free-outer }
    @free-levels.push($v);
}
rec-free(3);
$free-outer = 200;
is $free-alias, 200,
    'captured free variable bound from a deep recursion still tracks its source';
is @free-levels, (0, 1, 2, 3),
    'a free-variable bind leaves every recursion level own local untouched';

# Element binds (`$x := @a[0]` / `$x := %h<k>`) inside a recursive routine.
my @elem-levels;
sub rec-elem(Int $n) {
    my @a = ($n, $n + 10);
    if $n > 0 { rec-elem($n - 1) } else { my $x := @a[0]; $x = 999 }
    @elem-levels.push(@a[0]);
}
rec-elem(3);
is @elem-levels, (999, 1, 2, 3),
    'array-element := bind in a recursive routine touches only its own level';

my @hash-levels;
sub rec-hash(Int $n) {
    my %h = k => $n;
    if $n > 0 { rec-hash($n - 1) } else { my $x := %h<k>; $x = 999 }
    @hash-levels.push(%h<k>);
}
rec-hash(3);
is @hash-levels, (999, 1, 2, 3),
    'hash-element := bind in a recursive routine touches only its own level';

# No recursion needed for the same clobber: one routine, one caller, a
# same-named lexical in both. The routine's own `my $q` is bound to an alias
# and written through, and the write used to leak out to the caller's `$q`
# (todo/tickets/routine-local-bind-writes-through-to-same-named-outer-lexical.md).
my $outer-q = 'OUT';
sub routine-local-bind { my $outer-q = 5; my $r := $outer-q; $r = 9; $outer-q }
is routine-local-bind(), 9,
    'a routine-local := alias writes through inside its own routine';
is $outer-q, 'OUT',
    'a routine-local := alias does not leak the write to a same-named caller lexical';

# NOT covered here (still divergent, tracked in
# todo/tickets/bind-alias-chain-through-raw-params-blocks-innermost-frame-splice.md):
# the same bind performed from a CLOSURE nested inside the recursive routine.
# The closure's own compiled code has no slot for `$v`, so the gate above
# cannot see that the source is the enclosing invocation's own lexical, and the
# splice falls back to the name match across every recursion level.
