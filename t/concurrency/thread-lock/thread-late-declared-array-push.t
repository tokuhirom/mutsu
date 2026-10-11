use v6;
use Test;

# #11343: an `@` lexical declared AFTER a `start` was spawned, pushed to by a
# closure that the worker runs (a Supply tap callback), must be the declaring
# frame's array. The worker used to append into a name-keyed shared-store entry
# it created for itself, so the push succeeded on the worker and was lost to
# everyone else.

plan 8;

{
    my $s = Supplier.new;
    start { sleep 0.05; $s.emit("hi"); $s.done }
    my @got;
    my $p = Promise.new;
    $s.Supply.tap(-> $t { @got.push($t) }, done => -> { $p.keep });
    await Promise.anyof($p, Promise.in(10));
    is-deeply @got, ["hi"], 'tap callback push from the worker reaches a later-declared array';
}

{
    my $s = Supplier.new;
    start { sleep 0.05; $s.emit("hi"); $s.done }
    my @got;
    my $p = Promise.new;
    $s.Supply.tap(-> $t { push @got, $t }, done => -> { $p.keep });
    await Promise.anyof($p, Promise.in(10));
    is-deeply @got, ["hi"], 'the sub form `push @got, $t` too';
}

{
    my $s = Supplier.new;
    sub f {
        my $done = Promise.new;
        start { sleep 0.05; $s.emit("x"); $s.done }
        my @b;
        $s.Supply.tap(-> $t { @b.push($t) }, done => { $done.keep });
        await Promise.anyof($done, Promise.in(10));
        @b.elems
    }
    is f(), 1, 'same inside a routine';
}

{
    my $s = Supplier.new;
    start { sleep 0.05; $s.emit($_) for 1..3; $s.done }
    my @front;
    my @back;
    my $p = Promise.new;
    $s.Supply.tap(
        -> $t { @front.unshift($t); @back.append($t, $t * 10) },
        done => -> { $p.keep });
    await Promise.anyof($p, Promise.in(10));
    is-deeply @front, [3, 2, 1], 'unshift from the worker reaches a later-declared array';
    is-deeply @back, [1, 10, 2, 20, 3, 30], 'append from the worker reaches a later-declared array';
}

{
    # Several workers append to the same late-declared array through one
    # closure; the lock keeps the expectation valid under rakudo too.
    my $go = Promise.new;
    my @workers = (^4).map: -> $w { start { await $go; $w } };
    my @all;
    my $lock = Lock.new;
    my &add = -> $v { $lock.protect: { @all.push($v) } };
    $go.keep;
    await @workers.map: -> $p { $p.then({ add($_) for ^50 }) };
    is @all.elems, 200, 'appends from several threads all land';
    is @all.grep(* == 0).elems, 4, 'and each one landed once';
}

{
    # Removal methods share the same home as the appends: a worker that pushes
    # and shifts must see its own pushes, and the declaring frame the result.
    my $go = Promise.new;
    my @workers = (^4).map: -> $w { start { await $go; $w } };
    my @q = ^10;
    my $lock = Lock.new;
    my &churn = -> $v {
        $lock.protect: {
            @q.push($v, $v);
            @q.shift;
            @q.pop;
            @q.splice(0, 1, 'x');
        }
    };
    $go.keep;
    await @workers.map: -> $p { $p.then({ churn($_) for ^25 }) };
    is @q.elems, 10, 'push/shift/pop/splice from several threads keep one array';
}
