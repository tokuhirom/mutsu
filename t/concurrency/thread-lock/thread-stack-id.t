use Test;

# `$*STACK-ID` names the call stack the code runs on: 0 in the mainline, and a
# distinct Int for every `start` block, `.then` callback and thread (#11269).

plan 8;

is $*STACK-ID, 0, 'the mainline is stack 0';
isa-ok $*STACK-ID, Int, 'it is an Int';

sub f { $*STACK-ID }
is f(), 0, 'a sub called from the mainline runs on the same stack';

my @ids = await (^4).map: { start { $*STACK-ID } };
ok @ids.all > 0, 'each start block reads a non-zero id';
is @ids.unique.elems, 4, 'each start block is a stack of its own';

my ($a, $b) = await start { ($*STACK-ID, f()) };
is $a, $b, 'the id is stable within one start block';

my $then = await Promise.kept(1).then({ $*STACK-ID });
ok $then != 0 && $then ∉ @ids, 'a .then callback gets a new id';

my $in-thread;
my $t = Thread.start({ $in-thread = $*STACK-ID });
$t.finish;
ok $in-thread > 0, 'a Thread gets a non-zero id';
