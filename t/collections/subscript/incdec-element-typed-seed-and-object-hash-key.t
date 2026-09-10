use v6;
use Test;

# `++`/`--` on a container ELEMENT must
#   (1) seed an absent element from the container's declared element type
#       (Raku's `postfix:<++>` is `.=succ` on the current value, and a Bool
#       increments to True, not to Int 1), and
#   (2) key an object hash (`my %h{KeyType}`) by the key's `.WHICH`, recording
#       the key object so `.keys`/`.kv` report the object -- exactly as an `=`
#       store and an element read already do.

plan 19;

# --- (1) element seed comes from the declared element type -------------------

my Bool:D %b;
%b<a>++;
is %b<a>, True, 'Bool-valued hash element ++ yields True';
isa-ok %b<a>, Bool, 'Bool-valued hash element ++ stays a Bool';
%b<a>++;
is %b<a>, True, 'incrementing a True Bool element stays True';

my Bool @ba;
@ba[0]++;
is @ba[0], True, 'Bool-valued array element ++ yields True';

my Num %n;
%n<q>++;
is %n<q>, 1e0, 'Num-valued hash element ++ yields 1e0';
isa-ok %n<q>, Num, 'Num-valued hash element ++ stays a Num';

my Int %i;
%i<z>++;
is %i<z>, 1, 'Int-valued hash element ++ still yields 1';

my %plain;
%plain<x>++;
%plain<x>++;
is %plain<x>, 2, 'untyped hash element ++ is unchanged';

# --- (2) object-hash element ++ keys by .WHICH -------------------------------

my %s{Str};
%s<a>++;
%s<a>++;
is %s<a>, 2, 'Str-keyed object hash accumulates across ++';
is %s.elems, 1, 'Str-keyed object hash ++ uses one bucket';
is-deeply %s.keys.list, ("a",), 'Str-keyed object hash reports its key';

my %oi{Int};
%oi{5}++;
%oi{5}++;
is %oi{5}, 2, 'Int-keyed object hash accumulates across ++';
is-deeply %oi.keys.list, (5,), 'Int-keyed object hash reports the Int key object';

my %d{Date};
my $day = Date.new(2026, 9, 7);
%d{$day}++;
is %d{$day}, 1, 'Date-keyed object hash element ++ is readable back';
is-deeply %d.keys.list, ($day,), 'Date-keyed object hash reports the Date key object';

# An object hash whose values are Bool combines both halves: the key object has
# to survive AND the element has to seed from `Bool`.
my Bool:D %seen{Array:D};
my Str:D @path = <alpha beta>;
%seen{$@path}++;
is %seen.elems, 1, 'Array-keyed Bool object hash got one entry';
ok %seen.keys.first ~~ Positional, 'Array-keyed object hash key stays a Positional';
is %seen.keys.first.join('|'), 'alpha|beta', 'the recorded key object is the path array';
is %seen.values.first, True, 'Bool-valued object hash element ++ yields True';

done-testing;
