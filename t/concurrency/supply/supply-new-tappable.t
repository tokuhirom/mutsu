use Test;

# `Supply.new($tappable)` builds a supply over a user `Tappable` (rakudo's
# `method new(Tappable)`; EEL's `MatcherSupply`, #16888): each tap calls the
# tappable's `tap(&emit, &done, &quit, &tap)`, and the supply is an ordinary
# Supply for its methods and for `whenever`. Any other argument list still
# throws X::Supply::New.

plan 9;

class Counter does Tappable {
    has $.upto;
    has $.taps = 0;
    method tap(&emit, &done, &quit, &tap) {
        $!taps++;
        tap(Tap.new({;}));
        emit($_) for 1..$!upto;
        done();
        Tap.new({;})
    }
    method live { False }
    method sane { True }
}

my $counter = Counter.new(:upto(3));
my $s = Supply.new($counter);
isa-ok $s, Supply, 'Supply.new over a Tappable is a Supply';

my @seen;
my $done = False;
$s.tap(-> $v { @seen.push($v) }, done => { $done = True });
is-deeply @seen, [1, 2, 3], 'a tap sees what the tappable emits';
ok $done, 'and its done';

is-deeply $s.map(* * 10).list, (10, 20, 30), 'Supply methods drive it';
is $counter.taps, 2, 'each tap calls the tappable\'s tap again';

my @w;
react { whenever $s -> $v { @w.push($v) } }
is-deeply @w, [1, 2, 3], 'whenever taps it';

class Failing does Tappable {
    method tap(&emit, &done, &quit, &tap) {
        emit('a');
        quit(X::AdHoc.new(:payload<boom>));
        Tap.new({;})
    }
    method live { False }
    method sane { True }
}
my $quit;
Supply.new(Failing.new).tap(-> $ { }, quit => { $quit = .message });
is $quit, 'boom', 'quit reaches the tap\'s quit';

throws-like { Supply.new }, X::Supply::New, 'Supply.new with no tappable';
throws-like { Supply.new(:foo) }, X::Supply::New, 'Supply.new with a named argument only';
