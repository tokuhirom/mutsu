use Test;
use nqp;

# annotations::containers::Buffer.STAMP: `+@values is raw` handed each element to a
# Binder with `$binder.VAR.BIND: $bindee`, relying on the elements still being the
# caller's containers (`nqp::isrwcont`). The single-argument-rule slurpy (`+@a`) bound
# plain values, so a later assignment through the slot was silently dropped; `*@a is raw`
# already aliased them.

plan 10;

sub rw-flags(+@v is raw) { @v.map: -> Mu $x is raw { nqp::isrwcont($x) } }

my $s = 5;
is-deeply rw-flags($s).list, (1,), 'a variable argument binds its container';
is-deeply rw-flags($ = 1).list, (1,), 'an anonymous scalar argument binds its container';
is-deeply rw-flags($s, $s).list, (1, 1), 'several variable arguments';

sub set-first(+@v is raw) { @v[0] = 9 }
set-first($s);
is $s, 9, 'assigning to the element writes the caller variable';

sub bump-all(+@v is raw) { for @v -> $x is raw { $x++ } }
my ($p, $q) = 1, 2;
bump-all($p, $q);
is "$p $q", '2 3', 'each variable is aliased';

my @a = 1, 2, 3;
bump-all(@a);
is-deeply @a, [2, 3, 4], 'an array source aliases its elements';

# `*@a is raw` is unchanged.
sub star-bump(*@v is raw) { for @v -> $x is raw { $x++ } }
my $t = 1;
star-bump($t);
is $t, 2, '*@a is raw still aliases';

# Not raw: values are copied.
sub plain(+@v) { @v[0] = 100; 1 }
my $u = 1;
plain($u);
is $u, 1, '+@a without is raw does not alias';

# The Binder shape: a slot binds the container it was handed and assigns through it later.
class Slot {
    has $!value is default(Nil);
    method bind(Mu $topic is raw) { $!value := $topic }
    method set($v) { $!value = $v }
    method get { $!value }
}
sub stamp(+@values is raw) {
    my @slots;
    for @values -> Mu $b is raw { my $slot = Slot.new; $slot.bind($b); @slots.push($slot) }
    @slots
}
my @slots = stamp($ = 1, $ = 2);
@slots[0].set(7);
is-deeply @slots.map(*.get).list, (7, 2), 'a slot assigns through the container it bound';

# A lone itemized array is one element.
sub count(+@v is raw) { @v.elems }
is count($[1, 2, 3]), 1, 'an itemized array is a single element';
