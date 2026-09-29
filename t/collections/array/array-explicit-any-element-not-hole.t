use Test;

# An element explicitly assigned `Any` is data, not a hole: it must not read
# back as the array's default (`Mu` for a `Mu`-typed array, or the
# `is default(...)` value). Found via BSON::Simple, whose `Hash::Ordered`
# (`has Mu @.values`) decoded a BSON null `Any` as `Mu`.

plan 9;

my Mu @a;
@a[0] = Any;
is @a[0].raku, 'Any', 'Mu @a: assigned Any reads back Any';
is @a.AT-POS(0).raku, 'Any', 'Mu @a: AT-POS of assigned Any';
@a.ASSIGN-POS(1, Any);
is @a.AT-POS(1).raku, 'Any', 'Mu @a: ASSIGN-POS of Any';

my @d is default(42);
@d[0] = Any;
@d[2] = 1;
is @d[0].raku, 'Any', 'is default: assigned Any stays Any';
is @d[1], 42, 'is default: a real hole reads the default';
@d[0]:delete;
is @d[0], 42, 'is default: a deleted slot reads the default';

my @e is default(7);
@e = Nil, Any, 1;
is @e.raku, '[7, Any, 1]', 'list assignment: Nil decays to default, Any stays';

my Mu @m = Any, 1;
is @m[0].raku, 'Any', 'Mu @m initializer keeps Any';

my Int @i;
@i[2] = 1;
is @i[0].raku, 'Int', 'typed array hole still reads the element type';
