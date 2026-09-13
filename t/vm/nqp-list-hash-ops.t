use v6;
use Test;
use nqp;

# The untyped list/hash `nqp::` ops (`push`, `pop_*`, `shift_*`, `bindpos`,
# `hash`, `chr`), the two `p6*` bridge ops, the lazy `nqp::ifnull` special form,
# and `nqp::create` of the storage types. Measured against the `raku` oracle.
#
# Upstream `JSON::Fast`'s scanner and serializer are written almost entirely in
# these, and `nqp::push` / `nqp::hash` / `nqp::p6bindattrinvres` were also the
# whole of #8215 (13 zef distributions). See #8226.

plan 43;

# -- nqp::push / nqp::pop / nqp::shift, untyped --------------------------------

my $l := nqp::list();
nqp::push($l, 42);
nqp::push($l, "x");
is nqp::elems($l), 2, 'nqp::push appends';
is nqp::atpos($l, 0), 42, 'nqp::push stored the value untyped (Int)';
is nqp::atpos($l, 1), "x", 'nqp::push stored the value untyped (Str)';
is nqp::pop($l), "x", 'nqp::pop returns the last element';
is nqp::elems($l), 1, 'nqp::pop removed it';
is nqp::shift($l), 42, 'nqp::shift returns the first element';
is nqp::elems($l), 0, 'nqp::shift removed it';

my $s := nqp::list_s("a", "b", "c");
is nqp::pop_s($s), "c", 'nqp::pop_s returns the last element as a str';
is nqp::shift_s($s), "a", 'nqp::shift_s returns the first element as a str';
is nqp::elems($s), 1, 'both ends were consumed';

my $i := nqp::list_i(7, 8, 9);
is nqp::shift_i($i), 7, 'nqp::shift_i returns the first element as an int';
is nqp::pop_i($i), 9, 'nqp::pop_i returns the last element as an int';

# -- nqp::bindpos: a sparse lookup table --------------------------------------

# JSON::Fast decodes a hex digit by indexing a table built exactly this way, so
# an unset slot has to read back falsy.
my $hex := nqp::list();
nqp::bindpos($hex, 65, 10);  # A
nqp::bindpos($hex, 97, 10);  # a
is nqp::atpos($hex, 65), 10, 'nqp::bindpos stores past the end';
is nqp::atpos($hex, 97), 10, 'nqp::bindpos stores past the end again';
# A gap, and an index past the end, have to read back as something falsy —
# `JSON::Fast` decodes a hex digit by branching on exactly that. Tested through
# `nqp::if` rather than `.Bool` / a native-int assignment because rakudo's
# unbound slot is a `VMNull`, which has neither.
is nqp::if(nqp::atpos($hex, 65), "hit", "gap"), "hit", 'a bound slot reads back truthy';
is nqp::if(nqp::atpos($hex, 66), "hit", "gap"), "gap",
    'a gap left by nqp::bindpos reads back falsy';
is nqp::if(nqp::atpos($hex, 500), "hit", "gap"), "gap",
    'an index past the end reads back falsy';

# -- nqp::hash, nqp::chr ------------------------------------------------------

my $h := nqp::hash('a', 1, 'b', 2);
is nqp::atkey($h, 'a'), 1, 'nqp::hash binds alternating key/value arguments';
is nqp::elems($h), 2, 'nqp::hash has both pairs';
is nqp::chr(9731), "\x[2603]", 'nqp::chr builds the one-character string';

# -- the p6 bridge ops --------------------------------------------------------

# `JSON::Fast` builds every parsed hash/array element through this, taking the
# descriptor off the container it is filling.
my %descriptor-source;
my $descriptor := nqp::getattr(%descriptor-source, Hash, '$!descriptor');
is nqp::decont(nqp::p6scalarwithvalue($descriptor, 7)), 7,
    'nqp::p6scalarwithvalue yields the value it was given';

# ... and it ITEMIZES an aggregate, which is the observable half of rakudo's
# "wrap it in a fresh Scalar". A value bound in through raw `nqp::bindkey`
# never reaches the store-side hook that itemizes an ordinary `%h<k> = [1,2]`,
# so without this `JSON::Fast`'s decoded objects came back un-itemized:
# `from-json('{"a":[1,2]}')<a>.raku` answered `[1, 2]` where rakudo, running
# that same module, answers `$[1, 2]`.
#
# Which kinds itemize is measured against rakudo, and is narrower than mutsu's
# own element-store itemization -- a Range does NOT itemize here.
is nqp::p6scalarwithvalue($descriptor, [1, 2]).raku, '$[1, 2]',
    'nqp::p6scalarwithvalue itemizes an Array';
is nqp::p6scalarwithvalue($descriptor, {:c(3)}).raku, '${:c(3)}',
    'nqp::p6scalarwithvalue itemizes a Hash';
is nqp::p6scalarwithvalue($descriptor, (1, 2)).raku, '$(1, 2)',
    'nqp::p6scalarwithvalue itemizes a List';
is nqp::p6scalarwithvalue($descriptor, (1, 2).Seq).raku, '$((1, 2).Seq)',
    'nqp::p6scalarwithvalue itemizes a Seq';
is nqp::p6scalarwithvalue($descriptor, 1 .. 3).raku, '1..3',
    'nqp::p6scalarwithvalue leaves a Range alone';
is nqp::p6scalarwithvalue($descriptor, (a => 1)).raku, ':a(1)',
    'nqp::p6scalarwithvalue leaves a Pair alone';
is nqp::p6scalarwithvalue($descriptor, True).raku, 'Bool::True',
    'nqp::p6scalarwithvalue leaves a Bool alone';
is nqp::p6scalarwithvalue($descriptor, Any).raku, 'Any',
    'nqp::p6scalarwithvalue leaves a type object alone';

# The end-to-end shape this came from -- a decoded JSON object's values
# itemizing -- is pinned by t/collections/element-store-itemization.t, which is
# where the regression surfaced. It is not repeated here: this file is kept
# runnable under `prove -e raku` for parity, and rakudo has no JSON::Fast
# installed.

class Chainable {
    has $!x;
    method set($v) { nqp::p6bindattrinvres(self, Chainable, '$!x', $v) }
    method x { $!x }
}
is Chainable.new.set(42).x, 42, 'nqp::p6bindattrinvres returns the invocant';

# -- nqp::ifnull is LAZY ------------------------------------------------------

my $evaluated = 0;
sub bump() { $evaluated++; "fallback" }
is nqp::ifnull("present", bump()), "present",
    'nqp::ifnull yields its first operand when that is not null';
is $evaluated, 0, 'nqp::ifnull did NOT evaluate the second operand';
is nqp::ifnull(nqp::null, bump()), "fallback", 'nqp::ifnull falls back when null';
is $evaluated, 1, 'nqp::ifnull evaluated the second operand exactly once';

# -- nqp::create allocates the STORAGE of a storage type ----------------------

my class IterationMap is repr("VMHash") { }
my $vmhash := nqp::create(IterationMap);
nqp::bindkey($vmhash, 'k', 'v');
is nqp::atkey($vmhash, 'k'), 'v', "nqp::create of an is-repr('VMHash') class is writable";

my class Raw is repr("VMArray") { }
my $vmarray := nqp::create(Raw);
nqp::push($vmarray, 1);
is nqp::elems($vmarray), 1, "nqp::create of an is-repr('VMArray') class is writable";

my $buffer := nqp::create(IterationBuffer);
nqp::push($buffer, "one");
is nqp::elems($buffer), 1, 'nqp::create(IterationBuffer) gives a pushable buffer';

# -- installing a storage object into a List/Map -----------------------------

# `hllize-list`: the buffer is filled FIRST, then installed — the List must end
# up holding what the buffer already had.
my $filled := nqp::create(IterationBuffer);
nqp::push($filled, 7);
nqp::push($filled, 8);
my $list := nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $filled);
is-deeply $list.List, (7, 8), 'a pre-filled IterationBuffer installs into a List';
isa-ok $list, List, 'and the result is a List';

# `parse-array`: the buffer is installed FIRST and pushed onto afterwards — the
# two have to be one store from the bind onwards.
my @result;
nqp::bindattr(@result, List, '$!reified', my $late := nqp::create(IterationBuffer));
nqp::push($late, "a");
nqp::push($late, "b");
is-deeply @result, ["a", "b"],
    'pushing onto an installed IterationBuffer reaches the Array it was installed into';

my $storage := nqp::create(IterationMap);
nqp::bindkey($storage, 'x', 1);
my $map := nqp::p6bindattrinvres(nqp::create(Map), Map, '$!storage', $storage);
is $map<x>, 1, 'a VMHash storage installs into a Map';
isa-ok $map, Map, 'and the result is a Map';

# `parse-obj`: reading `'$!storage'` off a Hash hands the hash itself back, so
# the `nqp::ifnull` fallback never fires and bindkey builds the hash in place.
my %obj;
my $obj-storage := nqp::ifnull(
  nqp::getattr(%obj, Map, '$!storage'),
  nqp::bindattr(%obj, Map, '$!storage', nqp::hash)
);
nqp::bindkey($obj-storage, 'k', 9);
is %obj<k>, 9, "nqp::bindkey through a Hash's own '\$!storage' builds it in place";

# vim: ft=perl6
