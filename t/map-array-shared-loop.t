use Test;

plan 21;

# A read-only `.map` block over a concrete array runs through the shared
# compile-once/`run_reuse` map loop (`eval_map_over_items_rw`), not the
# per-element closure-call loop in `vm/vm_native_map.rs` (which is now reserved
# for `$_`-mutating / `is rw` blocks that need the source writeback). These pin
# that an Array and a List give identical results for every block shape the
# routing change moved, so the two loops cannot drift apart.

my @arr = 1, 2, 3;
my $lst = (1, 2, 3);

is-deeply @arr.map({ $_ * 2 }).List, $lst.map({ $_ * 2 }).List, 'bare block: array matches list';
is-deeply @arr.map(* * 2).List, $lst.map(* * 2).List, 'WhateverCode: array matches list';
is-deeply @arr.map(-> $x { $x * 2 }).List, $lst.map(-> $x { $x * 2 }).List,
    'pointy block: array matches list';
is-deeply @arr.map({ $_.succ }).List, $lst.map({ $_.succ }).List,
    'method call in block: array matches list';
is-deeply @arr.map({ $^a + 1 }).List, $lst.map({ $^a + 1 }).List,
    'placeholder block: array matches list';

# `$_` inside a WhateverCode stays the CALLER's topic (the element binds to the
# `*` placeholder). Only a bare block topicalizes to the element. The shared
# loop routes this through `bind_loop_topic`; it used to bind the element
# unconditionally, so `* eq $_` compared each element against itself.
{
    my @w = <x y>;
    $_ = 'y';
    is-deeply @w.map(* eq $_).List, (False, True), 'WhateverCode $_ is the outer topic';
    is-deeply @w.map({ $_ eq 'y' }).List, (False, True), 'bare block topicalizes to the element';
    is-deeply @w.map(-> $c { $_ }).List, ('y', 'y'), 'pointy block leaves $_ as the outer topic';
    # A `$_`-reading WhateverCode must not write the outer topic back into the
    # source array.
    @w.map(* eq $_).List;
    is-deeply @w.List, ('x', 'y'), 'outer-topic map leaves the source untouched';
}

# Shapes the shared loop has to keep handling for arrays.
is-deeply @arr.map({ $_ == 2 ?? slip($_, $_) !! $_ }).List, (1, 2, 2, 3),
    'Slip from a map block flattens';
is-deeply @arr.map({ next if $_ == 2; $_ }).List, (1, 3), 'next skips an element';
is-deeply @arr.map({ last if $_ == 3; $_ }).List, (1, 2), 'last stops the loop';
my @whened = @arr.map({
    given $_ {
        when 2 { 'two' }
        default { $_ }
    }
});
is-deeply @whened.List, (1, 'two', 3), 'when/default inside a map block';

my @pairs = 'a' => 1, 'b' => 2;
is-deeply @pairs.map({ .key }).List, ('a', 'b'),
    'Pair elements bind as the topic, not as named args';

my @even = 1, 2, 3, 4;
is-deeply @even.map(-> $a, $b { $a + $b }).List, (3, 7), 'multi-arity block chunks the source';

# `.map` still returns a Seq, and the source is untouched by a read-only block.
isa-ok @arr.map({ $_ }), Seq, 'map returns a Seq';
@arr.map({ $_ + 100 });
is-deeply @arr.List, (1, 2, 3), 'read-only map leaves the source unchanged';

# A block closing over an outer lexical still sees the live value.
my $bump = 10;
is-deeply @arr.map({ $_ + $bump }).List, (11, 12, 13), 'captured outer lexical is visible';

# The writeback cases still work (these keep the vm/vm_native_map.rs loop).
{
    my @m = 1, 2, 3;
    @m.map({ $_++ });
    is-deeply @m.List, (2, 3, 4), '$_-mutating block still writes back';
}
{
    my @m = 1, 2, 3;
    @m.map(-> $x is rw { $x++ });
    is-deeply @m.List, (2, 3, 4), 'is-rw param still writes back';
}
{
    my Int @t = 1, 2, 3;
    @t.map({ $_++ });
    is @t.WHAT.gist, '(Array[Int])', 'rw writeback keeps the element type';
}
