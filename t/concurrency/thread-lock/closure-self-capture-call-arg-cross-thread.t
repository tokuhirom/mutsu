use Test;

# `my $t = $supply.tap({ ...; $t.close })`: the closure is created inside the
# declaration's own initializer, before `$t` is bound, and it is only a call
# argument -- yet the callee keeps it and runs it later, possibly on another
# thread. It must see the value the declaration stored, which only a shared
# cell can carry (#9493, Chronic `t/040-at.t`).

plan 5;

sub keep-and-run-later(&c) { start { sleep 0.2; c() } }

{
    my @saved;
    sub keep(&c) { @saved.push(&c); 42 }
    my $th = start { sleep 0.2; @saved[0]() };
    my $t = keep({ $t });
    is (await $th), 42, 'a call-argument closure run on an earlier-started thread sees its own declaration';
}

sub in-a-routine() {
    my @saved;
    my sub keep(&c) { @saved.push(&c); 'stored' }
    my $th = start { sleep 0.2; @saved[0]() };
    my $t = keep({ $t });
    await $th
}
is in-a-routine(), 'stored', '... also inside a routine body';

{
    my $p = keep-and-run-later({ $p.^name });
    is (await $p), 'Promise', 'a closure passed to a routine that runs it later sees the declaration';
}

{
    my $seen = Promise.new;
    my $v = $seen.vow;
    my $tap = Supply.interval(0.05).tap({
        $v.keep($tap.^name) if $seen.status ~~ Planned;
        $tap.close;
    });
    await Promise.anyof($seen, Promise.in(10));
    is $seen.result, 'Tap', 'an interval tap callback sees the Tap it is assigned to';
}

class Waiter {
    method wait-for(Supplier $s) {
        my $p = Promise.new;
        my $v = $p.vow;
        my $tap = $s.Supply.tap({ $tap.close; $v.keep($_) });
        start { $s.emit(True) };
        $p
    }
}
my $s = Supplier.new;
is (await Promise.anyof(Waiter.wait-for($s), Promise.in(10))), True,
    'a method body closing its own tap from the callback completes';
