use Test;

# MCP::Client::Leases waits with `sleep($deadline - now)` (a Duration). mutsu read a
# Duration as "no number" and slept forever.

plan 8;

my $d = (now + 0.15) - now;
is $d.WHAT.raku, 'Duration', 'now - now is a Duration';

my $t = now;
sleep($d);
my $slept = now - $t;
ok 0.1 <= $slept < 3, 'sleep takes a Duration as seconds';

$t = now;
sleep(Duration.new(0.1));
ok now - $t < 3, 'a constructed Duration';

# The wait loop of Leases.
my $deadline = now + 0.5;
my $ticks = 0;
loop {
    my $left = $deadline - now;
    last if $left <= 0;
    sleep($left < 0.2 ?? $left !! 0.2);
    $ticks++;
}
ok 2 <= $ticks <= 4, 'a deadline loop sleeping the remaining Duration terminates';

my $rest = sleep-timer(Duration.new(0.05));
is $rest.WHAT.raku, 'Duration', 'sleep-timer takes a Duration and answers the remainder';
is +$rest, 0, 'nothing left';

is +Duration.new(2.5), 2.5, 'a Duration numifies';
is Duration.new(0.5) + 1, 1.5, 'and does arithmetic';
