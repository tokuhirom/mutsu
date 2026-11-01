use Test;
use nqp;

plan 35;

class Bound {
    has $.value is built(:bind);
    method store($new) { $!value = $new }
}

my $source = 5;
my $object = Bound.new(value => $source);
$object.store(9);
is $source, 9, 'native construction binds the caller Scalar';
$source = 11;
is $object.value, 11, 'later caller writes remain visible';

my @sources = 1, 2;
my $element = Bound.new(value => @sources[0]);
$element.store(7);
is @sources[0], 7, 'an array element retains its own container';

my %sources = key => 3;
my $entry = Bound.new(value => %sources<key>);
$entry.store(8);
is %sources<key>, 8, 'a hash element retains its own container';

throws-like { Bound.new(value => 7).store(1) }, X::AdHoc,
    message => /immutable/, 'a literal binds as an immutable value';

class Assigned {
    has $.value;
    method store($new) { $!value = $new }
}
my $plain = 2;
my $assigned = Assigned.new(value => $plain);
$assigned.store(4);
is $plain, 2, 'ordinary construction still assigns into its own Scalar';
$plain = 6;
is $assigned.value, 4, 'ordinary attribute does not follow caller stores';

my $default = 12;
class DefaultBound {
    has $.value is built(:bind) = $default;
    method store($new) { $!value = $new }
}
my $with-default = DefaultBound.new;
$with-default.store(13);
is $default, 13, 'a bound default retains its declaration container';
$default = 14;
is $with-default.value, 14, 'bound default observes subsequent caller writes';

class LiteralDefault {
    has $.value is built(:bind) = 42;
    method store($new) { $!value = $new }
}
throws-like { LiteralDefault.new.store(1) }, X::AdHoc,
    message => /immutable/, 'a literal default remains immutable';

class Blessed {
    has $.value is built(:bind);
    method new(*%args) { self.bless(|%args) }
    method store($new) { $!value = $new }
}
my $through-bless = 20;
my $blessed = Blessed.new(value => $through-bless);
$blessed.store(21);
is $through-bless, 21, 'a custom new forwards the container through bless';

for ^2 {
    my $repeated = 30;
    Bound.new(value => $repeated).store(31);
    is $repeated, 31, 'cached construction retains this call container';
}

my $escaped;
my $read-escaped;
{
    my $local = 40;
    $escaped = Bound.new(value => $local);
    $read-escaped = -> { $local };
}
$escaped.store(41);
is $read-escaped(), 41, 'the shared bound cell survives its declaring frame';

my $nil = Nil;
my $bound-nil = Bound.new(value => $nil);
$bound-nil.store(50);
is $nil, 50, 'an undefined caller Scalar remains writable';

class Planned {
    has $.value is built(:bind);
    method store($new) { $!value = $new }
}
my \plan := nqp::getattr(Planned.HOW, Metamodel::ClassHOW, '@!BUILDALLPLAN');
nqp::bindattr(Planned.HOW, Metamodel::ClassHOW, '@!BUILDALLPLAN', plan);
my $planned-source = 60;
my $planned = Planned.new(value => $planned-source);
$planned.store(61);
is $planned-source, 61, 'replacement BUILDALLPLAN binds the caller container';
$planned-source = 62;
is $planned.value, 62, 'replacement plan retains the same cell';
throws-like { Planned.new(value => 7).store(1) }, X::AdHoc,
    message => /immutable/, 'replacement plan preserves immutable binding';

my $deferred-source = 70;
class Deferred {
    has $.value is built(:bind) = $deferred-source;
    submethod BUILD() { }
    method store($new) { $!value = $new }
}
my $deferred = Deferred.new;
$deferred.store(71);
is $deferred-source, 71, 'post-BUILD default binds its source';
$deferred-source = 72;
is $deferred.value, 72, 'post-BUILD binding retains its source cell';

class Built {
    has $.value is built(:bind) = 100;
    submethod BUILD() { $!value = 80 }
    method store($new) { $!value = $new }
}
my $built = Built.new;
$built.store(81);
is $built.value, 81, 'a BUILD write owns the slot instead of the bound default';

class Typed {
    has Int $.value is built(:bind);
    method store($new) { $!value = $new }
}
throws-like { Typed.new(value => 'wrong') }, X::TypeCheck,
    'binding checks the initial attribute type';
my $typed = Typed.new;
$typed.store(90);
is $typed.value, 90, 'an unprovided typed attribute keeps its writable seed';

class Native {
    has int $.value is built(:bind);
    method store($new) { $!value = $new }
}
my int $native = 1;
Native.new(value => $native).store(2);
is $native, 1, 'native attributes retain ordinary assignment semantics';

my $array-holder = [1, 2];
Bound.new(value => $array-holder).store(3);
is $array-holder, 3, 'a Scalar holding an Array retains its Scalar container';
my $hash-holder = { key => 1 };
Bound.new(value => $hash-holder).store(4);
is $hash-holder, 4, 'a Scalar holding a Hash retains its Scalar container';

my $proxy-value = 5;
my $proxy := Proxy.new(
    FETCH => method () { $proxy-value },
    STORE => method ($new) { $proxy-value = $new },
);
Bound.new(value => $proxy).store(6);
is $proxy-value, 6, 'a bound Proxy retains its STORE protocol';

# An `is List` aggregate needs the generic constructor rather than its native
# fast path; the scalar binding contract is identical in both engines.
my $generic-default = 7;
class Generic {
    has @.items is List;
    has $.value is built(:bind) = $generic-default;
    method store($new) { $!value = $new }
}
my $generic-source = 8;
Generic.new(value => $generic-source).store(9);
is $generic-source, 9, 'generic construction binds a provided container';
Generic.new.store(10);
is $generic-default, 10, 'generic construction binds the default container';

class Inherited is Bound { }
my $inherited = 11;
Inherited.new(value => $inherited).store(12);
is $inherited, 12, 'an inherited bound attribute keeps its construction mode';
class OrdinaryChild is Bound {
    has $.value;
    method store($new) { $!value = $new }
}
my $child-source = 13;
OrdinaryChild.new(value => $child-source).store(14);
is $child-source, 13, 'a new ordinary declaration does not inherit the parent binding trait';

sub from-readonly($value) { Bound.new(value => $value) }
throws-like { from-readonly(15).store(16) }, X::AdHoc,
    message => /readonly/, 'a readonly parameter retains its readonly binding';

my Int $typed-source = 100;
my $typed-binding = Bound.new(value => $typed-source);
throws-like { $typed-binding.store('wrong') }, X::TypeCheck,
    'writes through an untyped attribute enforce the source container type';
is $typed-source, 100, 'a rejected store leaves the source unchanged';
throws-like { Bound.new(value => Int).store(1) }, X::AdHoc,
    message => /concrete/, 'a bound type object retains the concrete-object refusal';
