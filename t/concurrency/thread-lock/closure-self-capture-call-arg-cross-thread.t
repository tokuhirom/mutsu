use Test;

# `my $t = $supply.tap({ ...; $t.close })`: the closure is created inside the
# declaration's own initializer, before `$t` is bound, and it is only a call
# argument -- yet the callee keeps it and runs it later, possibly on another
# thread. It must see the value the declaration stored, which only a shared
# cell can carry (#9493, Chronic `t/040-at.t`).

plan 5;

sub keep-and-run-later(&c, Promise:D $gate) { start { await $gate; c() } }

{
    my @saved;
    my $ready = Promise.new;
    sub keep(&c) { @saved.push(&c); 42 }
    my $th = start { await $ready; @saved[0]() };
    my $t = keep({ $t });
    $ready.keep;
    is (await $th), 42, 'a call-argument closure run on an earlier-started thread sees its own declaration';
}

sub in-a-routine() {
    my @saved;
    my $ready = Promise.new;
    my sub keep(&c) { @saved.push(&c); 'stored' }
    my $th = start { await $ready; @saved[0]() };
    my $t = keep({ $t });
    $ready.keep;
    await $th
}
is in-a-routine(), 'stored', '... also inside a routine body';

{
    my $gate = Promise.new;
    my $p = keep-and-run-later({ $p.^name }, $gate);
    $gate.keep;
    is (await $p), 'Promise', 'a closure passed to a routine that runs it later sees the declaration';
}

{
    my $seen = Promise.new;
    my $v = $seen.vow;
    my $s = Supplier.new;
    my $tap = $s.Supply.tap({
        $v.keep($tap.^name) if $seen.status ~~ Planned;
        $tap.close;
    });
    start { $s.emit(True) };
    await Promise.anyof($seen, Promise.in(10));
    is $seen.result, 'Tap', 'an asynchronous tap callback sees the Tap it is assigned to';
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
