use v6;
use Test;

# PLAN 8.5 step 1: .List materializes array holes as literal Nil
# (Rakudo semantics; .Slip keeps Any / the `is default` value instead),
# argless .head reads the store raw (hole -> Nil), and a List element
# holding literal Nil satisfies `=:= Nil`.

plan 16;

# --- .List on an autovivification-gap array ---
my @a;
@a[2] = 5;
is @a.List.raku, '(Nil, Nil, 5)', '.List materializes autoviv holes as Nil';
is @a.List[0].raku, 'Nil', 'a .List hole element reads back as literal Nil';
ok @a.List[0] =:= Nil, 'a .List hole element is =:= Nil';
is @a.head.raku, 'Nil', 'argless .head on a hole gives Nil, not Any';
is @a[0].raku, 'Any', 'a direct element read still vivifies to Any';
is @a.Slip.raku, 'slip(Any, Any, 5)', '.Slip keeps holes as Any';

# --- deleted slots are holes too ---
my @b = 1, 2, 3;
@b[1]:delete;
is @b.List.raku, '(1, Nil, 3)', '.List materializes a deleted slot as Nil';
ok @b.List[1] =:= Nil, 'a deleted slot reads as Nil through .List';

# --- `is default(...)` does not change .List (only .Slip) ---
my @c is default(42) = 1, 2, 3;
@c[1]:delete;
is @c.List.raku, '(1, Nil, 3)', '.List keeps holes Nil even with is default';
is @c.Slip[1], 42, '.Slip substitutes the is default value for holes';

# --- typed arrays: holes still materialize as Nil ---
my Int @d;
@d[2] = 5;
is @d.List.raku, '(Nil, Nil, 5)', '.List on a typed array materializes holes as Nil';
is @d.head.raku, 'Nil', '.head on a typed-array hole gives Nil';

# --- literal Nil in a List preserves identity ---
my $l = (Nil, 1);
is $l[0].raku, 'Nil', 'a literal Nil List element reads back as Nil';
ok $l[0] =:= Nil, 'a literal Nil List element is =:= Nil';
ok (Nil, 1)[0] =:= Nil, 'an anonymous List Nil element is =:= Nil';

# --- .head(n) iterates, so holes vivify to Any (unlike argless .head) ---
my @e = 1, 2, 3;
@e[1]:delete;
is @e.head(3).raku, '(1, Any, 3).Seq', '.head(n) iteration keeps holes as Any';
