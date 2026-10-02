use Test;

# An explicit `Nil` initializer on a sigilless term or a `constant` is a
# user-written value, not the parser's synthesized "no initializer" default,
# so it must stay Nil instead of being re-seeded as Any (#10554).

plan 10;

my \x = Nil;
is x.raku, 'Nil', 'my \x = Nil binds Nil';
ok x === Nil, 'my \x = Nil is identical to Nil';

my \y := Nil;
is y.raku, 'Nil', 'my \y := Nil binds Nil';

{
    my \inner = Nil;
    is inner.raku, 'Nil', 'block-scoped sigilless Nil';
}

constant z = Nil;
is z.raku, 'Nil', 'constant z = Nil (package scope) is Nil';

my constant m = Nil;
is m.raku, 'Nil', 'my constant m = Nil is Nil';

constant $c = Nil;
is $c.raku, 'Nil', 'constant $c = Nil is Nil';

constant \q = Nil;
is q.raku, 'Nil', 'constant \q = Nil is Nil';

# The untouched paths keep their own defaults.
my $plain;
is $plain.raku, 'Any', 'an uninitialized $ scalar still holds Any';
my $explicit = Nil;
is $explicit.raku, 'Any', 'assigning Nil to a $ scalar still resets it to Any';
