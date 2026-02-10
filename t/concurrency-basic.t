use Test;
plan 8;

my $p = Promise.new();
is $p.status, "Planned", 'promise starts planned';
$p.keep(42);
is $p.result, 42, 'promise result after keep';
is $p.status, "Kept", 'promise status kept';

my $p2 = $p.then(-> $x { $x + 1 });
is $p2.result, 43, 'promise then transforms result';

my $c = Channel.new();
$c.send(1);
$c.send(2);
is $c.receive, 1, 'channel receive first';
is $c.receive, 2, 'channel receive second';
$c.close;
ok $c.closed, 'channel closed';

my $s = Supply.new();
my $sum = 0;
$s.tap(-> $x { $sum += $x });
$s.emit(2);
$s.emit(3);
is $sum, 5, 'supply tap receives emitted values';
