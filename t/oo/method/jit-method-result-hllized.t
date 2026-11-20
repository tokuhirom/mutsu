use Test;
use nqp;

plan 2;

# A method call's result is HLL-ized (a raw VMHash reads as a Hash), also once
# the enclosing code is compiled by the JIT: the JIT's call-site helpers used to
# hand the raw value back (nightly `jit-stress-tap`, MUTSU_JIT_THRESHOLD=2).
my $h = { message => "deep" };
my sub storage-type(@a) {
    @a.map({ nqp::getattr(nqp::decont($_), Map, '$!storage') }).head.WHAT.^name
}

my %seen;
%seen{storage-type([$h])}++ for ^3000;
is %seen.keys.sort.join(','), 'Hash', 'a method result is a Hash on every call';

my sub via-method(@a) { my $v = @a.map({ nqp::getattr(nqp::decont($_), Map, '$!storage') }).head; $v.WHAT.^name }
my %seen2;
%seen2{via-method([$h])}++ for ^3000;
is %seen2.keys.sort.join(','), 'Hash', 'same through a stored method result';
