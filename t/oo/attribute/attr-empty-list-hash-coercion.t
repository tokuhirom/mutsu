use v6;
use Test;

# A `%`-sigil attribute must coerce its init value to a Hash exactly like
# `my %h = <value>`. Non-empty pair lists already coerced, but an *empty* list
# (or `Empty`) was kept as a List/Slip, so a later `%!attr<key>` died with
# "Type Array does not support associative indexing". This surfaced from
# HTTP::MediaType's `has %.parameters` when a media type carried no parameters
# (`text/html` with no `; charset=...`).

plan 12;

class C { has %.parameters; }

# --- empty list / Empty / default all yield an (indexable) empty Hash ---
my $a = C.new(parameters => ());
ok $a.parameters ~~ Hash, 'empty-list %-attr init is a Hash';
is $a.parameters.elems, 0, '...and it is empty';
is ($a.parameters<charset> // 'undef'), 'undef', '...and associative indexing works';

my $b = C.new(parameters => Empty);
ok $b.parameters ~~ Hash, 'Empty %-attr init is a Hash';
is ($b.parameters<charset> // 'undef'), 'undef', 'Empty %-attr indexes fine';

my $d = C.new;
ok $d.parameters ~~ Hash, 'defaulted %-attr is a Hash';
is ($d.parameters<charset> // 'undef'), 'undef', 'defaulted %-attr indexes fine';

# --- a non-pair even list pairs up key => value, like `my %h = 1,2,3,4` ---
my $e = C.new(parameters => (1, 2, 3, 4));
ok $e.parameters ~~ Hash, 'flat even list %-attr is a Hash';
is $e.parameters<1>, 2, 'alternating list pairs key=>value (1 => 2)';
is $e.parameters<3>, 4, 'alternating list pairs key=>value (3 => 4)';

# --- a pair list still coerces (unchanged) ---
my $f = C.new(parameters => (charset => 'utf-8'));
is $f.parameters<charset>, 'utf-8', 'pair-list %-attr still coerces';

# --- the @-sigil sibling was already correct; keep it green ---
class D { has @.a; }
ok D.new(a => ()).a ~~ Array, 'empty-list @-attr is an Array';
