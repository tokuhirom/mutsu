use Test;

# `Supply.tap(:tap(&cb))` hands `cb` the Tap before the supply produces
# anything; closing it there stops a supply block before its body runs
# (#16888, EEL 0.3's "immediate cancellation" checks).

plan 14;

my @log;
supply { emit 1; emit 2 }.tap(-> $v { @log.push("emit $v") },
    tap => -> $h { @log.push('tap') }, done => { @log.push('done') });
is-deeply @log, ['tap', 'emit 1', 'emit 2', 'done'], 'the tap callback runs first';

@log = ();
my $seen;
my $returned = supply { emit 1 }.tap(-> $v { @log.push($v) }, tap => -> $h { $seen = $h });
isa-ok $seen, Tap, 'the callback receives a Tap';
ok $seen === $returned, 'the same Tap .tap returns';

my $ran = 0;
my $count = 0;
supply { $ran++; emit 1; emit 2 }.tap(-> $v { $count++ }, tap => -> $h { $h.close });
is $ran, 0, 'a supply block closed in the callback never runs';
is $count, 0, 'and emits nothing';

my $mapped = 0;
supply { emit 1; emit 2 }.map(* * 2).tap(-> $v { $mapped++ }, tap => -> $h { $h.close });
is $mapped, 0, 'nor does one behind a map';

my $listed = 0;
(1, 2, 3).Supply.tap(-> $v { $listed++ }, tap => -> $h { $h.close });
is $listed, 3, 'a list supply emits all the same, as in rakudo';

my $supplier = Supplier.new;
my @live;
my $live-tap = $supplier.Supply.tap(-> $v { @live.push($v) }, tap => -> $h { @live.push('tap') });
$supplier.emit(5);
$live-tap.close;
$supplier.emit(6);
is-deeply @live, ['tap', 5], 'a live supply: the callback first, then values until closed';

my $closed-live = 0;
$supplier.Supply.tap(-> $v { $closed-live++ }, tap => -> $h { $h.close });
$supplier.emit(7);
is $closed-live, 0, 'a live tap closed in the callback receives nothing';

my $on-close = 0;
my $plain = Tap.new({ $on-close++ });
$plain.close;
$plain.close;
is $on-close, 2, 'Tap.new(&on-close) runs its block on every close';

class Upper does Tappable {
    method live { False }
    method sane { True }
    method serial { True }
    method tap(&emit, &done, &quit, &tap) {
        my $closed = False;
        my $answer = Tap.new({ $closed = True });
        tap($answer);
        for 1, 2, 3 { emit($_) unless $closed }
        done() unless $closed;
        $answer
    }
}

my @upper;
my $handed;
Supply.new(Upper.new).tap(-> $v { @upper.push($v) }, tap => -> $h { $handed = $h });
is-deeply @upper, [1, 2, 3], 'a Tappable supply emits';
isa-ok $handed, Tap, "the :tap callback gets the Tappable's Tap";

my @stopped;
Supply.new(Upper.new).tap(-> $v { @stopped.push($v) }, tap => -> $h { $h.close });
is-deeply @stopped, [], "closing it there stops the Tappable";

class Relay does Tappable {
    has Supply $.source;
    method live { True }
    method sane { True }
    method serial { True }
    method tap(&emit, &done, &quit, &tap) {
        my $closed = False;
        my $answer = Tap.new({ $closed = True });
        $!source.tap(-> $v { emit($v) unless $closed }, done => { done() unless $closed });
        tap($answer);
        $answer
    }
}
my $source = Supplier.new;
my ($relayed, $finished) = 0, 0;
my $relay-tap = Supply.new(Relay.new(source => $source.Supply)).tap(-> $v { $relayed++ }, done => { $finished++ });
$source.emit(1);
$relay-tap.close;
$source.emit(2);
$source.done;
is ($relayed, $finished), (1, 0), "closing the returned Tap closes the Tappable's";
