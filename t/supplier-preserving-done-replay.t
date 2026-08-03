use v6;
use Test;

plan 9;

# A Supplier::Preserving keeps its whole replay list -- the buffered values AND
# the terminal `done` -- past `.done`, so a tap made afterwards still sees both.
# The terminal is handed out exactly once, like the values are.

# 1) done before any tap: the first late tap replays values, then done.
{
    my $p = Supplier::Preserving.new;
    $p.emit(1);
    $p.emit(2);
    $p.done;
    my @got;
    my $done = 0;
    $p.Supply.tap: { @got.push($_) }, done => { $done++ };
    is @got.join(","), "1,2", 'a tap made after done still replays the backlog';
    is $done, 1, 'and then sees done';

    my @late;
    my $late-done = 0;
    $p.Supply.tap: { @late.push($_) }, done => { $late-done++ };
    is @late.elems, 0, 'a second late tap gets no values';
    is $late-done, 0, 'and no done -- the replay list was already consumed';
}

# 2) done with nothing buffered: the first late tap still gets done.
{
    my $p = Supplier::Preserving.new;
    $p.done;
    my $done = 0;
    $p.Supply.tap: -> $ { }, done => { $done++ };
    is $done, 1, 'an empty preserving supplier hands its done to the first late tap';
}

# 3) done delivered live: later taps see nothing at all.
{
    my $p = Supplier::Preserving.new;
    my $live-done = 0;
    $p.Supply.tap: -> $ { }, done => { $live-done++ };
    $p.emit(1);
    $p.done;
    is $live-done, 1, 'the live tap gets done';
    my $late-done = 0;
    my @late;
    $p.Supply.tap: { @late.push($_) }, done => { $late-done++ };
    is @late.elems + $late-done, 0,
        'a tap after a live done sees neither values nor done';
}

# 4) A `whenever` on an already-done preserving supply runs its LAST phaser:
#    the terminal reaches the subscription, not just the buffered values.
{
    my $p = Supplier::Preserving.new;
    $p.emit(10);
    $p.emit(20);
    $p.done;
    my @got;
    my $out = supply {
        whenever $p.Supply -> $v {
            @got.push($v);
            LAST { @got.push("last") }
        }
    }
    my $out-done = 0;
    $out.tap: -> $ { }, done => { $out-done++ };
    is @got.join(","), "10,20,last",
        'a whenever on a finished preserving supply replays values then LAST';
    is $out-done, 1, 'and the enclosing supply completes';
}
