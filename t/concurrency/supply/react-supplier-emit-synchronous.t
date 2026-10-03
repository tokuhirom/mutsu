use Test;

# A live Supplier delivers synchronously: `emit`/`done` return only after a
# `react` tapping it -- even one running on another thread -- has run its
# `whenever` (issue #11268).

plan 6;

{
    # Emitted right after the react body tapped the supplier, before its event
    # loop started.
    my @got;
    my $s = Supplier.new;
    my $ready = Promise.new;
    start react { whenever $s.Supply -> $m { @got.push($m) }; $ready.keep };
    await $ready;
    $s.emit($_) for 1..3;
    is +@got, 3, 'emit returns after a react on another thread handled the value';
    $s.emit(4);
    is-deeply @got.List, (1, 2, 3, 4), 'a later emit is handled before it returns too';
}

{
    my @log;
    my $s = Supplier.new;
    my $ready = Promise.new;
    my $react = start react {
        whenever $s.Supply {
            @log.push("value $_");
            LAST { @log.push('last') }
        }
        $ready.keep;
    };
    await $ready;
    $s.emit(1);
    $s.done;
    is-deeply @log.List, ('value 1', 'last'), 'done returns after the LAST phaser ran';
    await $react;
}

{
    # Many producers at once: every value is handled by the time they finish.
    my @got;
    my $s = Supplier.new;
    my $ready = Promise.new;
    start react { whenever $s.Supply { @got.push($_) }; $ready.keep };
    await $ready;
    await (^20).map: -> $w { start { $s.emit($w * 10 + $_) for ^10 } };
    is +@got, 200, 'values from concurrent producers are all handled when they return';
}

{
    # Two reacts on different threads feeding each other: a handler that emits
    # into a react which is itself waiting on this one must not deadlock.
    my $a = Supplier.new;
    my $b = Supplier.new;
    my @seen;
    my $ready-a = Promise.new;
    my $ready-b = Promise.new;
    start react {
        whenever $a.Supply -> $n { @seen.push("a$n"); $b.emit($n) }
        $ready-a.keep;
    };
    start react {
        whenever $b.Supply -> $n { @seen.push("b$n"); $a.emit($n + 1) if $n < 2 }
        $ready-b.keep;
    };
    await $ready-a, $ready-b;
    $a.emit(0);
    my $deadline = now + 10;
    sleep 0.01 while @seen < 6 && now < $deadline;
    is-deeply @seen.sort.List, <a0 a1 a2 b0 b1 b2>, 'reacts emitting into each other do not deadlock';
}

{
    # A handler that blocks releases the producer instead of holding it.
    my $s = Supplier.new;
    my $gate = Promise.new;
    my $handled = Promise.new;
    my $ready = Promise.new;
    start react {
        whenever $s.Supply { await $gate; $handled.keep($_) }
        $ready.keep;
    };
    await $ready;
    my $emitter = start { $s.emit(42); 'returned' };
    await Promise.anyof($emitter, Promise.in(10));
    $gate.keep;
    is (await $handled), 42, 'a producer is not held while the handler blocks';
}
