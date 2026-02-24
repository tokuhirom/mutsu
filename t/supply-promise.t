use Test;
plan 7;

dies-ok { Supply.Promise }, "Supply.Promise cannot be called as class method";

my $s = Supplier.new;
my $p = $s.Supply.Promise;
isa-ok $p, Promise, "Supplier.Supply.Promise returns a Promise";
is $p.status, Planned, "Promise starts Planned";

$s.emit(41);
is $p.status, Planned, "Promise stays Planned until done";

$s.emit(42);
$s.done;
is $p.status, Kept, "Promise is Kept after done";
is $p.result, 42, "Promise result is the last emitted value";

my $s2 = Supplier.new;
my $p2 = $s2.Supply.Promise;
$s2.quit("oops");
is $p2.status, Broken, "Promise is Broken after quit";
