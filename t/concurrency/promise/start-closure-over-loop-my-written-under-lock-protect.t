use Test;

# MCP::Client::Test::InProcessTransport.request: a per-request `my $done`, claimed under
# `$lock.protect: { $mine = !$done; $done = True }` by a `work` closure that `start`
# runs, and by a `Promise.in(...).then` timer. The spawned block only calls the closure
# (`start { $work() }`), so the variable the closure writes was not among the block's own
# free variables and stayed on the name-keyed cross-thread lane: a worker of one
# iteration resolved `$done` to another iteration's binding, saw `True`, and never kept
# its promise (the request, and the `await` on it, hung).

plan 6;

sub run-batch(&make, Int $n = 40) {
    my @promises = (1..$n).map(&make);
    my $deadline = now + 10;
    sleep 0.05 while now < $deadline && @promises.grep({ .status eq 'Planned' }).elems;
    @promises.grep({ .status ne 'Kept' }).elems;
}

is run-batch(-> $i {
    my $promise = Promise.new;
    my $vow = $promise.vow;
    my $lock = Lock.new;
    my $done = False;
    my $work = {
        my $mine = False;
        $lock.protect: { $mine = !$done; $done = True };
        $vow.keep($i) if $mine;
    };
    start { $work() };
    $promise;
}), 0, 'a closure called through a variable by start claims its own $done';

is run-batch(-> $i {
    my $promise = Promise.new;
    my $vow = $promise.vow;
    my $lock = Lock.new;
    my $done = False;
    my &work = {
        my $mine = False;
        $lock.protect: { $mine = !$done; $done = True };
        $vow.keep($i) if $mine;
    };
    start work();
    $promise;
}), 0, 'also with a named closure and start work()';

# Each worker must see the $done its own iteration declared.
my @entered;
my @workers;
for 1..60 -> $i {
    my $lock = Lock.new;
    my $done = False;
    my $work = {
        my $before = $done;
        $lock.protect: { $done = True };
        @entered.push($i) if $before;
    };
    @workers.push(start { $work() });
}
await @workers;
is @entered.elems, 0, 'no worker starts with a $done another iteration already set';

# The closure nested two deep (the shape of settle -> act).
is run-batch(-> $i {
    my $promise = Promise.new;
    my $vow = $promise.vow;
    my $settled = Lock.new;
    my Bool $done = False;
    my &settle = -> &act {
        my Bool $mine = False;
        $settled.protect: { $mine = !$done; $done = True };
        act() if $mine;
    };
    my &work = { settle({ $vow.keep($i) }) };
    start work();
    $promise;
}), 0, 'a settle/act pair, as the transport writes it';

# A timer racing the handler for the same vow: exactly one of them wins, every time.
sub race-one(Int $i) {
    my $promise = Promise.new;
    my $vow = $promise.vow;
    my $settled = Lock.new;
    my Bool $done = False;
    my &settle = -> &act {
        my Bool $mine = False;
        $settled.protect: { $mine = !$done; $done = True };
        act() if $mine;
    };
    Promise.in(0.05).then({ settle({ $vow.break(X::AdHoc.new(payload => "timeout $i")) }) });
    my &work = { settle({ $vow.keep("answer $i") }) };
    start work();
    $promise;
}
my @raced = (1..30).map({ race-one($_) });
sleep 0.4;
is @raced.grep({ .status eq 'Planned' }).elems, 0, 'every raced request is settled one way or the other';

# A plain scalar the spawned block captures itself is unchanged.
my $plain = 0;
await (1..4).map({ start { $plain = 7 } });
is $plain, 7, 'a scalar the block writes directly is still shared';
