use Test;

# A Proc::Async output tap used to be delivered only by awaiting *that* process's
# own promise, because the replay hook fired on an `await` whose result was a
# `Proc`. `Promise.anyof`/`allof` resolve to a plain `True`, so the
# "run it, but give up after N seconds" idiom -- which is how Test::Util's
# `doesn't-hang` waits for the child -- saw an empty tap buffer.

plan 4;

sub run-and-collect(&wait-for) {
    my $p = Proc::Async.new: $*EXECUTABLE.absolute, '-e', 'say "B"';
    my $out = '';
    $p.stdout.tap: -> $chunk { $out ~= $chunk };
    my $started = $p.start;
    wait-for($started);
    $out;
}

is run-and-collect(-> $pr { await Promise.anyof: Promise.in(30), $pr }), "B\n",
    'a tap is delivered by awaiting a Promise.anyof over the process promise';

is run-and-collect(-> $pr { await Promise.allof: $pr }), "B\n",
    'a tap is delivered by awaiting a Promise.allof over the process promise';

is run-and-collect(-> $pr { (Promise.anyof: Promise.in(30), $pr).result }), "B\n",
    '.result on the composite delivers it too';

# `whenever Promise.allof(...)` must still settle only once every source has --
# the composite registry is shared with that driver.
my $order = '';
react {
    my $a = start { $order ~= 'a' };
    my $b = start { $order ~= 'b' };
    whenever Promise.allof($a, $b) {
        $order ~= '!';
        done;
    }
}
is $order.chars, 3, 'whenever Promise.allof still waits for every source';
