use Test;

# A parallel/lazy sequence RHS (`.hyper` / `.race` / `Slip`) assigned to a
# typed array must reify its elements and type-check each — not reject the whole
# HyperSeq/RaceSeq against the element type. Regression: `zef fetch` builds
# `my Candidate @fetched = @candidates.hyper(...).map(...)` (Zef::Client).

plan 11;

class C { has $.n }

# Declaration with initializer (goes through the TypeCheck opcode).
my C @a = (^3).hyper.map({ C.new(n => $_) });
is @a.elems, 3, 'HyperSeq -> typed array declaration: elems';
is @a.map(*.n).sort.join(','), '0,1,2', 'HyperSeq -> typed array declaration: elements type-checked and stored';

my C @b = (^3).race.map({ C.new(n => $_) });
is @b.elems, 3, 'RaceSeq -> typed array declaration: elems';

my C @c = (^4).hyper(:batch(1), :degree(2)).map({ C.new(n => $_) });
is @c.elems, 4, 'HyperSeq with :batch/:degree (the zef shape): elems';

# Plain assignment (goes through the SetLocal / coerce path, not TypeCheck).
my C @d;
@d = (^3).hyper.map({ C.new(n => $_) });
is @d.elems, 3, 'HyperSeq -> typed array assignment: elems';

# Native element type.
my Int @e = (^5).hyper.map({ $_ * 2 });
is @e.elems, 5, 'HyperSeq -> native Int typed array: elems';
is @e.sum, 20, 'HyperSeq -> native Int typed array: values';

my Int @f = (^5).race.map(* + 1);
is @f.sort.join(','), '1,2,3,4,5', 'RaceSeq -> native Int typed array: values';

# A type mismatch must still be caught after reification.
my $died = False;
try {
    my Int @g = (^3).hyper.map({ "not an int" });
    CATCH { default { $died = True } }
}
ok $died, 'HyperSeq element type mismatch is still rejected after reification';

# Untyped array baseline still works.
my @h = (^3).hyper.map({ C.new(n => $_) });
is @h.elems, 3, 'HyperSeq -> untyped array still works';

# Slip into a typed array.
my Int @i = (|(1, 2, 3),);
is @i.elems, 3, 'Slip into a native typed array reifies its elements';
