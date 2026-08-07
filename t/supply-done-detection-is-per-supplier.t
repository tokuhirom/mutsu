use Test;

# Tapping a `supply { ... }` block runs its body once and then asks "did the
# body complete this supply by calling `done`?". That question used to be
# answered from a process-global count of `Supplier.done` calls, so a `done`
# raised on a completely unrelated supplier -- on another thread, at any point
# while the body ran -- was mistaken for this block's own completion. The tap
# then immediately tore down the upstream taps it had just registered, and
# every later value was silently dropped.
#
# This is what made a Cro HTTP server lose about a quarter of the requests
# whenever a Cro client shared its process: the two pipelines call `done`
# constantly, so the server's freshly tapped per-connection chain closed itself
# before the request bytes could travel through it.

plan 2;

# A background thread that does nothing but create, tap and complete its own
# suppliers -- pure `Supplier.done` noise on another thread.
my $noise = start {
    for ^3000 {
        my $s = Supplier.new;
        $s.Supply.tap({ });
        $s.done;
    }
};

my $lost = 0;
for ^40 {
    my $src = Supplier.new;
    my $stage1 = supply { whenever $src.Supply -> $v { emit $v + 1 } };
    my $stage2 = supply { whenever $stage1 -> $v { emit $v * 2 } };
    my @got;
    $stage2.tap(-> $v { @got.push($v) });
    $src.emit(1);
    $src.emit(2);
    $lost++ unless @got eqv [4, 6];
}
await $noise;

is $lost, 0, 'a concurrent Supplier.done does not tear down a freshly tapped supply chain';

# The body's own `done` must still be detected: this supply completes itself,
# so its done handler fires.
my $completed = False;
my $self-done = supply { emit 1; done };
$self-done.tap(-> $v { }, done => { $completed = True });
ok $completed, 'a supply block that calls done itself still completes';
