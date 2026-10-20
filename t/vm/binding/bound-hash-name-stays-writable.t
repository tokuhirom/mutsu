use Test;

# A `%` name bound with `:=` stays writable: a whole assignment writes through
# to the bound hash, from the declaring scope and from a closure alike. A bound
# immutable value (a Map, a Mix) refuses it by its own type, and a `constant`
# hash keeps its readonly mark (ADR-0097, which retired the `__mutsu_bound::`
# key that used to tell a bound name from a constant one).

plan 9;

my %src = a => 1;
my %alias := %src;
%alias = (x => 2);
is-deeply %src, { x => 2 }, 'a whole assignment to a bound hash writes through';

my &assign = { %alias = (y => 3) };
assign();
is-deeply %src, { y => 3 }, '... from a closure too';

%alias<z> = 4;
is %src<z>, 4, 'an element store writes through';

sub rebind(%p) { my %q := %p; %q = (w => 5) }
rebind(%src);
is-deeply %src, { w => 5 }, 'a bound parameter alias writes through';

my %map := Map.new((a => 1));
throws-like { %map = (b => 2) }, X::Assignment::RO, 'a bound Map refuses a whole assignment';
is-deeply %map, Map.new((a => 1)), '... and keeps its contents';

my %mix := mix <a b b>;
throws-like { %mix = mix <c> }, X::Assignment::RO, 'a bound Mix refuses one from a closure';

constant %C = a => 1;
throws-like { EVAL '%C = ()' }, X::Assignment::RO, 'a constant hash stays readonly';

my %plain is Map = a => 1;
throws-like { %plain = (b => 2) }, X::Assignment::RO, 'an `is Map` hash refuses a whole assignment';
