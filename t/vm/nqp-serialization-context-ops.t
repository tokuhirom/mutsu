use Test;
use nqp;

# The serialization-context `nqp::` ops (#11504). Every expectation was
# measured against rakudo.

plan 26;

# createsc / scgethandle / scsetdesc / scgetdesc
my $sc := nqp::createsc('mutsu-sc-test');
is $sc.^name, 'SCRef', 'createsc answers an SCRef';
is nqp::scgethandle($sc), 'mutsu-sc-test', 'scgethandle answers the handle createsc was given';
ok nqp::eqaddr(nqp::createsc('mutsu-sc-test'), $sc), 'createsc with a known handle answers that same SC';
nok nqp::eqaddr(nqp::createsc('mutsu-sc-other'), $sc), 'another handle is another SC';
ok nqp::isnull_s(nqp::scgetdesc($sc)), 'a fresh SC has a null descriptor';
is nqp::scsetdesc($sc, 'a descriptor'), 'a descriptor', 'scsetdesc answers the descriptor';
is nqp::scgetdesc($sc), 'a descriptor', 'scgetdesc reads it back';

# scsetobj / scobjcount / scgetobjidx
is nqp::scobjcount($sc), 0, 'a fresh SC holds no objects';
my $first = [1, 2];
ok nqp::eqaddr(nqp::scsetobj($sc, 0, $first), $first), 'scsetobj answers the object';
is nqp::scobjcount($sc), 1, 'scobjcount counts it';
is nqp::scgetobjidx($sc, $first), 0, 'scgetobjidx finds it';
my %second = a => 1;
nqp::scsetobj($sc, 3, %second);
is nqp::scobjcount($sc), 4, 'an index past the end grows the root list';
is nqp::scgetobjidx($sc, %second), 3, 'scgetobjidx finds an object at a later index';
nqp::scsetobj($sc, 1, $first);
is nqp::scgetobjidx($sc, $first), 0, 'scgetobjidx answers the first index holding an unowned object';
throws-like { nqp::scgetobjidx($sc, [1, 2]) }, Exception,
    message => 'Object does not exist in serialization context',
    'an equal but distinct object is not in the SC';

# scsetcode keeps code refs apart from the root objects
my $code = sub { 42 };
ok nqp::eqaddr(nqp::scsetcode($sc, 5, $code), $code), 'scsetcode answers the code ref';
is nqp::scobjcount($sc), 4, 'scsetcode does not add a root object';

# setobjsc / getobjsc: the owning SC is a separate fact from membership
my $owned = [3];
ok nqp::eqaddr(nqp::setobjsc($owned, $sc), $owned), 'setobjsc answers the object';
ok nqp::eqaddr(nqp::getobjsc($owned), $sc), 'getobjsc answers the owning SC';
nqp::scsetobj($sc, 6, $owned);
nqp::scsetobj($sc, 7, $owned);
is nqp::scgetobjidx($sc, $owned), 7, 'an owned object answers the index it was last stored at';
ok nqp::isnull(nqp::getobjsc([3])), 'an equal but distinct object has no owning SC';

# pushcompsc / popcompsc
throws-like { nqp::popcompsc() }, Exception, message => 'No current compiling SC',
    'popcompsc on an empty stack dies';
my $other := nqp::createsc('mutsu-sc-other');
nqp::pushcompsc($sc);
ok nqp::eqaddr(nqp::pushcompsc($other), $other), 'pushcompsc answers the SC';
ok nqp::eqaddr(nqp::popcompsc(), $other), 'popcompsc pops the last SC pushed';
ok nqp::eqaddr(nqp::popcompsc(), $sc), '... then the one before it';

# a non-SC operand
throws-like { nqp::scgethandle(42) }, Exception,
    message => 'Must provide an SCRef operand to scgethandle',
    'an SC op refuses a non-SC operand';
