use Test;
plan 8;

my $p = Promise.new();
is $p.status, "Planned", 'promise starts planned';
$p.keep(42);
is $p.result, 42, 'promise result after keep';
is $p.status, "Kept", 'promise status kept';

# .then passes the Promise itself to the callback, not the result
my $p2 = $p.then(-> $x { $x.result + 1 });
is $p2.result, 43, 'promise then transforms result';

my $c = Channel.new();
$c.send(1);
$c.send(2);
is $c.receive, 1, 'channel receive first';
is $c.receive, 2, 'channel receive second';
$c.close;
ok $c.closed, 'channel closed';

my $sup = Supplier.new;
my $s = $sup.Supply;
my $sum = 0;
$s.tap(-> $x { $sum += $x });
$sup.emit(2);
$sup.emit(3);
is $sum, 5, 'supply tap receives emitted values';
