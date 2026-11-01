use Test;

plan 15;

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
{
    my $local = 40;
    $escaped = Bound.new(value => $local);
}
$escaped.store(41);
is $escaped.value, 41, 'the bound cell survives its declaring frame';

my $nil = Nil;
my $bound-nil = Bound.new(value => $nil);
$bound-nil.store(50);
is $nil, 50, 'an undefined caller Scalar remains writable';
