use Test;

# A tap closure inside a sub writes to the sub's own `my @log`, even when the
# caller's loop body has a same-named `my @log` and the supply was created
# before the sub's `@log` was declared (#11345). `use Test` makes mainline
# lexicals visible by name, which is what let the stale binding leak in.

plan 4;

sub rc() {
    my $in = Supplier.new;
    my $s = supply {
        whenever $in.Supply -> $m {
            whenever start { 'x' } -> $ { emit 'A' }
        }
    }
    my @log;
    my $all = Promise.new;
    $s.tap: -> $v { @log.push($v); $all.keep };
    $in.emit(1);
    await Promise.anyof($all, Promise.in(5));
    @log
}

for 1, 2 -> $n {
    my @log = rc();
    is @log.elems, 1, "call $n: the tap wrote to its own array";
}

# The same with the declaration after the supply and a timer-driven emit.
sub timed($n) {
    my $s = supply { whenever Promise.in(0.05) -> $ { emit "A$n" } }
    my @log;
    my $all = Promise.new;
    $s.tap: -> $v { @log.push($v); $all.keep };
    await Promise.anyof($all, Promise.in(5));
    @log
}

for 1, 2 -> $n {
    my @log = timed($n);
    is-deeply @log, ["A$n"], "timed call $n: only its own emission";
}
