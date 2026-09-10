use Test;

plan 5;

# A typed @-array declared in EXPRESSION position (assignment RHS, call
# argument) must tag its element-type metadata, so `.of` survives — the same as
# a statement-position declaration. Previously these reported `.of` = Mu.

my $x = (my Str @c);
is @c.of, Str, 'typed @-array declared in assignment RHS keeps .of';

sub id(\a) { a }
id(my Str @d);
is @d.of, Str, 'typed @-array passed as a call argument keeps .of';

# Element-type drives the missing-element subscript-adverb default.
is-deeply (@c[5]:!v), Str, 'missing-element :!v uses the element type';

# A raw-param mutator (the roast `gen` idiom) preserves the type.
sub gen(\a) { a[0] = 'x' }
gen my Str @e;
is @e.of, Str, 'typed @-array inline-declared as arg to a raw-param sub keeps .of';

# Statement-position still works (regression guard).
my Str @f;
is @f.of, Str, 'statement-position typed @-array keeps .of';
