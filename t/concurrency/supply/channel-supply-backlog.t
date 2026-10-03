use Test;

# `Channel.Supply` is an on-demand supply whose every tap is a consumer of the
# channel queue (rakudo: a `supply { }` that drains the backlog, then polls once
# per send). A value sent before any tap existed is still queued for the first
# tap, and a value leaves the queue for exactly one consumer -- a tap, a
# `receive`/`poll`, or another tap. Issue #9900: values sent before the first
# tap used to be lost, because `send` emitted them at send time into a supply
# nobody had tapped yet.
#
# Completion is observed through promises the `done`/`quit` handlers keep, so
# nothing here depends on how fast a scheduler thread runs.

plan 15;

sub collect($supply) {
    my @got;
    my $p = Promise.new;
    $supply.tap: { @got.push($_) },
        done => { $p.keep('done') },
        quit => -> $e { $p.keep("quit:{$e.message}") };
    await Promise.anyof($p, Promise.in(5));
    ($p.status ~~ Kept ?? $p.result !! 'timeout', @got.List)
}

# 1. The issue's repro: values sent before the tap are delivered first.
{
    my $c = Channel.new;
    my $s = $c.Supply;
    $c.send(1);
    $c.send(2);
    my @got;
    my $p = Promise.new;
    $s.tap({ @got.push($_) }, done => { $p.keep });
    $c.send(3);
    $c.close;
    await Promise.anyof($p, Promise.in(5));
    is-deeply @got.List, (1, 2, 3), 'values sent before the first tap are delivered';
}

# 2. A tap on a channel that is already closed drains it, then completes.
{
    my $c = Channel.new;
    $c.send(1);
    $c.close;
    is-deeply collect($c.Supply), ('done', (1,)), 'a closed channel drains to a late tap, then done';
}

# 3. ... and on a failed one it drains, then quits with the failure.
{
    my $c = Channel.new;
    $c.send(1);
    $c.fail('boom');
    is-deeply collect($c.Supply), ('quit:boom', (1,)), 'a failed channel drains, then quits';
}

# 4-5. Combinators tap the channel per use, so they see the backlog too.
{
    my $c = Channel.new;
    $c.send(1);
    $c.send(2);
    my $mapped = $c.Supply.map(* * 10);
    $c.send(3);
    $c.close;
    is-deeply collect($mapped), ('done', (10, 20, 30)), '.map sees the backlog';
}
{
    my $c = Channel.new;
    $c.send($_) for 1..4;
    $c.close;
    is-deeply collect($c.Supply.grep(* %% 2)), ('done', (2, 4)), '.grep sees the backlog';
}

# 6. `.list` on a closed channel's Supply returns what was queued.
{
    my $c = Channel.new;
    $c.send(1);
    $c.send(2);
    $c.close;
    is-deeply $c.Supply.list, (1, 2), '.list of a closed channel returns the backlog';
}

# 7. `await` on it gives the last value.
{
    my $c = Channel.new;
    $c.send(1);
    $c.send(2);
    $c.close;
    is await($c.Supply), 2, 'await of a closed channel Supply is the last value';
}

# 8. A `whenever` inside a `supply` block taps it too.
{
    my $c = Channel.new;
    $c.send(1);
    $c.send(2);
    my $doubled = supply { whenever $c.Supply { emit $_ * 2 } };
    $c.send(3);
    $c.close;
    is-deeply collect($doubled), ('done', (2, 4, 6)), 'a supply block over the channel sees the backlog';
}

# 9. The tap consumed the backlog: nothing is left for `poll`.
{
    my $c = Channel.new;
    $c.send(1);
    $c.send(2);
    my @got;
    $c.Supply.tap({ @got.push($_) });
    sleep 0.2;
    is-deeply (@got.List, $c.poll), ((1, 2), Nil), 'the backlog goes to the tap, not to a later poll';
}

# 10. A value goes to the tap OR stays in the queue, never both.
{
    my $c = Channel.new;
    my @got;
    my $t = $c.Supply.tap({ @got.push($_) });
    $c.send(1);
    sleep 0.2;
    $t.close;
    $c.send(2);
    is-deeply (@got.List, $c.poll, $c.poll), ((1,), 2, Nil),
        'a delivered value is not also queued; after Tap.close values stay queued';
}

# 11-12. The first tap takes the whole backlog; a second tap competes for later values.
{
    my $c = Channel.new;
    $c.send($_) for 1..3;
    my (@a, @b);
    $c.Supply.tap({ @a.push($_) });
    $c.Supply.tap({ @b.push($_) });
    sleep 0.2;
    is-deeply (@a.List, @b.List), ((1, 2, 3), ()), 'the first tap drains the backlog';
    $c.send($_) for 4..7;
    $c.close;
    sleep 0.3;
    is (flat @a, @b).sort.join(','), '1,2,3,4,5,6,7', 'later values are each delivered exactly once';
}

# 13. A `send` made from inside a `supply` body feeds the channel's tap and is
#     not mistaken for a value that body emitted.
{
    my $c = Channel.new;
    my @tap;
    $c.Supply.tap({ @tap.push($_) });
    my @outer;
    supply { $c.send(5); emit 1 }.tap({ @outer.push($_) });
    sleep 0.2;
    is-deeply (@tap.List, @outer.List), ((5,), (1,)), 'a send inside a supply body reaches only the channel tap';
}

# 14. A react `whenever` sees the backlog as well.
{
    my $c = Channel.new;
    $c.send(1);
    $c.send(2);
    $c.close;
    my @got;
    react { whenever $c.Supply { @got.push($_) } }
    is-deeply @got.List, (1, 2), 'react whenever drains the backlog';
}

# 15. A tap with no callback is still a consumer: later sends are taken off the
#     queue rather than left for `poll`.
{
    my $c = Channel.new;
    $c.Supply.tap;
    $c.send(1);
    sleep 0.2;
    is $c.poll, Nil, 'a callback-less tap still consumes the values';
}
