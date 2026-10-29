use Test;

# Red t/28-events: a `whenever` handler of a live react runs inside the
# emitting `Supplier.emit` call on rakudo, so `$*dyn` in the handler resolves
# through the emitter's dynamic scope, not the react creator's.
plan 3;

my $*V = "top";
my $s = Supplier.new;
my @seen;
my $t = start react whenever $s.Supply -> $e { @seen.push: "$e:$*V"; done if $e == 3 }
sleep 0.2;

sub emitter { my $*V = "inner"; $s.emit(1) }
emitter();
$s.emit(2);
sub via-start { my $*V = "started"; await start { $s.emit(3) } }
via-start();
await $t;

is @seen[0], "1:inner", 'handler sees the emitting sub\'s dynamic';
is @seen[1], "2:top", 'without a rebinding it sees the outer dynamic';
is @seen[2], "3:started", 'an emit from a start block carries the starter\'s dynamic';
