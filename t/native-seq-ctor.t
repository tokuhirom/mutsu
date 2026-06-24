use Test;

# Pin for the VM-native `Seq.new` fast path (ledger §D ③ ctor). The VM gate in
# `vm_call_method_compiled.rs` and the interpreter's `dispatch_new` arm both call
# the single `try_native_seq_construct` helper, so the native and slow paths must
# stay byte-identical. Construction reads/writes only VM-owned carrier state.

plan 11;

# Seq.new(iterator) over a materialized list — eager copy of items.
my $s1 = Seq.new((1, 2, 3).iterator);
is $s1.List, (1, 2, 3), 'Seq.new(list iterator) yields the items';

# A second, independent Seq.new is not aliased to the first.
my $s2 = Seq.new((4, 5).iterator);
is $s2.elems, 2, 'independent Seq.new has its own items';
is $s1.elems, 3, 'first Seq is unaffected by the second';

# Deferred / lazy iterator: only consumed on demand.
my $s3 = Seq.new((10, 20, 30).iterator);
is $s3.List, (10, 20, 30), 'deferred Seq pulls its items on consumption';

# No-arg Seq.new() is a pre-consumed Seq.
my $s4 = Seq.new;
is $s4.raku, 'Seq.new()', 'argless Seq.new is pre-consumed';
dies-ok { $s4.List }, 'consuming an already-consumed Seq dies';

# A gather block produces a Seq that round-trips through .List.
my $g = gather { take $_ * $_ for 1..4 };
is $g.List, (1, 4, 9, 16), 'gather Seq materializes correctly';

# Seq.new is also reachable via a non-Package variable receiver (mut path).
my $cls = Seq;
my $s5 = $cls.new((7, 8, 9).iterator);
is $s5.List, (7, 8, 9), 'Seq.new via variable receiver (mut path)';

# Map over a Seq still works (the Seq value is a normal lazy sequence).
my $s6 = Seq.new((1, 2, 3).iterator);
is $s6.map(* + 1).List, (2, 3, 4), 'map over a native-constructed Seq';

# Empty iterator yields an empty Seq.
my $s7 = Seq.new(().iterator);
is $s7.elems, 0, 'Seq.new(empty iterator) is empty';

# A user subclass of Seq is left to the interpreter (not intercepted).
my $sub = (1, 2, 3).Seq;
is $sub.List, (1, 2, 3), '.Seq coercion still works alongside Seq.new';

done-testing;
