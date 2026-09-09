use Test;

# A `whenever` callback body's compiled chunk is cached across emitted values
# (#7667). The four per-code-object mutations that used to force a fresh compile
# -- the supply-body mark, the emitter name, the vouched capture set, the
# inherited owned-lexical set -- are part of the cache key now, so the cached
# chunk must behave exactly as a freshly compiled one did.

plan 6;

# The callback's `emit` must keep reaching its OWN supply's emitter across many
# values, not a sibling instance's.
sub doubler(Supply $in) { supply { whenever $in -> $v { emit $v * 2 } } }
my $s1 = Supplier.new;
my @got;
doubler(doubler($s1.Supply)).tap({ @got.push($_) });
$s1.emit($_) for 1..5;
is @got, [4, 8, 12, 16, 20], 'a chained supply pair keeps its emitters straight over many values';

# A lexical the callback closes over stays shared across every invocation.
my $sup = Supplier.new;
my $total = 0;
my $out = supply { whenever $sup -> $v { $total += $v; emit $total } };
my @running;
$out.tap({ @running.push($_) });
$sup.emit($_) for 1..4;
is @running, [1, 3, 6, 10], 'a captured lexical accumulates across cached-chunk runs';
is $total, 10, 'and the outer binding sees the final value';

# `state` inside a callback restarts per code object, not per call.
my $sup2 = Supplier.new;
my @seen;
supply { whenever $sup2 -> $v { state $n = 0; $n++; emit $n } }.tap({ @seen.push($_) });
$sup2.emit('x') for ^4;
is @seen, [1, 2, 3, 4], 'a `state` variable in a whenever body persists across values';

# A nested whenever inside the callback still registers each time. The nested
# body runs when the promise is kept, so the emitted value is what proves it ran
# -- awaited through the outer supply rather than slept on.
my $sup3 = Supplier.new;
my @fired;
my $done = Promise.new;
my $sup3out = supply {
    whenever $sup3 -> $v {
        my $p = Promise.new;
        whenever $p { emit $v * 10 }
        $p.keep;
    }
};
$sup3out.tap({ @fired.push($_); $done.keep if @fired.elems == 3 });
$sup3.emit($_) for 1..3;
await Promise.anyof($done, Promise.in(5));
is @fired, [10, 20, 30], 'a nested whenever inside the callback fires once per value';

# LAST still runs when the source is done.
my $sup4 = Supplier.new;
my @tail;
supply { whenever $sup4 -> $v { emit $v; LAST emit 'done' } }.tap({ @tail.push($_) });
$sup4.emit(7); $sup4.emit(8); $sup4.done;
is @tail, [7, 8, 'done'], 'a LAST phaser in a whenever body still runs after many values';
