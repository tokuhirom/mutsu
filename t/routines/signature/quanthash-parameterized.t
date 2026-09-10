use Test;

# Parameterized QuantHash types (Set/Bag/Mix and their mutable variants):
# `.^parameterize`, key coercion at construction, key coercion on element
# access/assignment, and rebinding a container variable.

plan 25;

# --- .^parameterize is the metamodel form of Type[T] ---
ok Set.^parameterize(Str) =:= Set[Str], '.^parameterize(Str) equals Set[Str]';
ok Bag.^parameterize(Int()) =:= Bag[Int()], '.^parameterize(Int()) equals Bag[Int()]';
is Set.^parameterize(Str).^name, 'Set[Str]', '.^parameterize gives the right name';
ok SetHash.^parameterize(Int()).keyof =:= Int(), '.keyof of a parameterized type';

# --- Str parameter: a plain type constraint ---
is-deeply Set[Str].new(qw/a b c/).keys.sort.List, qw/a b c/, 'Set[Str] keeps strings';
ok Set[Str].keyof =:= Str, 'Set[Str].keyof is Str';

# --- Coercive Int() parameter coerces keys at construction ---
is-deeply Set[Int()].new(qw/1 2 4/).keys.sort.List, (1, 2, 4), 'Set[Int()] coerces keys to Int';
ok Set[Int()].keyof =:= Int(), 'Set[Int()].keyof is Int()';
throws-like { Set[Int()].new(<a b c>) }, X::Str::Numeric,
    'a bad Int() key throws X::Str::Numeric at construction';

# --- Coercive Date() parameter ---
is-deeply Set[Date()].new('2026-05-05').keys.head, '2026-05-05'.Date,
    'Set[Date()] coerces a key to a Date';
throws-like { Set[Date()].new(<a b c>) }, X::Temporal::InvalidFormat,
    'a bad Date() key throws X::Temporal::InvalidFormat at construction';

# --- isa-ok recognises the parameterized type ---
isa-ok Set[Str].new, Set[Str], 'Set[Str].new is a Set[Str]';
isa-ok Set[Int()].new(1, 2), Set[Int()], 'Set[Int()].new is a Set[Int()]';
nok Set[Str].new.isa(Set[Int]), 'Set[Str] is not a Set[Int]';

# --- Immutable variant: RO on assignment, key coercion on access ---
{
    my $type := Set.^parameterize(Int());
    isa-ok (my %qh := $type.new), $type, 'rebind inside isa-ok arg';
    throws-like { %qh{"a"} }, X::Str::Numeric, 'bad key access croaks (immutable)';
    throws-like { %qh{42} = 1 }, X::Assignment::RO, 'assignment to immutable croaks with RO';
}

# --- Mutable variant: key coercion on access and assignment ---
{
    my %qh := SetHash[Int()].new;
    lives-ok { %qh{42} = 1 }, 'can assign an Int key to a mutable SetHash[Int()]';
    ok %qh{"42"}, 'string "42" finds the coerced Int key 42';
    lives-ok { %qh{"666"} = 1 }, 'can assign a coercible string key';
    is-deeply %qh.keys.sort.List, (42, 666), 'stored keys are Ints';
    throws-like { %qh{"a"} }, X::Str::Numeric, 'bad key access croaks (mutable)';
    throws-like { %qh{"a"} = 1 }, X::Str::Numeric, 'bad key assignment croaks (mutable)';
}

# --- Rebinding a container variable that holds an immutable QuantHash ---
{
    my %qh := Set.new(1, 2);
    lives-ok { %qh := Set.new(3, 4) }, 'a %-variable holding an immutable Set can be rebound';
    is-deeply %qh.keys.sort.List, (3, 4), 'the rebind replaced the container';
}

# vim: expandtab shiftwidth=4
