use Test;

plan 3;

# `Promise(supply { ... })` must return a Planned promise immediately and
# drive the supply's whenevers on a background thread, not the calling
# thread. Driving inline deadlocks whenever the calling thread is also the
# one that must go on to satisfy the supply (e.g. a Cro response body
# resolved from inside the same handler that must still send `done` to the
# underlying stream — see
# todo/tickets/promise-supply-coercion-drives-react-on-calling-thread.md,
# now news/).

{
    # Minimal shape: the coercion itself must not block.
    my $s = Supplier.new;
    my $p = Promise(supply {
        my $acc = '';
        whenever $s.Supply -> $v { $acc ~= $v; LAST emit $acc; }
    });
    is $p.status, 'Planned', 'Promise(supply {...}) returns Planned immediately';
    start { $s.emit('a'); $s.emit('b'); $s.done; }
    is await($p), 'ab', 'the background drive still resolves the promise correctly';
}

{
    # The deadlock shape: the SAME thread that must eventually `.done` the
    # source supplier is the one blocking on the coerced promise's result.
    # Driving the coercion inline here would deadlock forever; the fix
    # returns a Planned promise so `await` blocks on the returned promise
    # instead of inside the coercion.
    my $s = Supplier.new;
    my $result;
    my $done = Promise.new;
    start {
        my $p = Promise(supply {
            my $acc = '';
            whenever $s.Supply -> $v { $acc ~= $v; LAST emit $acc; }
        });
        $s.emit('x');
        $s.emit('y');
        $s.done;
        $result = await $p;
        $done.keep(True);
    }
    await Promise.anyof($done, Promise.in(5));
    ok $done.status eq 'Kept' && $result eq 'xy',
        'same-thread producer + coercion-awaiter does not deadlock';
}
