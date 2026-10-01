use Test;

# A `for` over an EXPRESSION that yields a List's bare values binds read-only
# items, exactly as a loop over a named immutable List does
# (for-immutable-list-source-readonly.t, #10349). Every expectation was
# measured against rakudo (#10397).

plan 16;

throws-like { $_ = 5 for (1,2).values }, X::AdHoc, 'literal .values topic is read-only';
throws-like { $_ = 5 for (1,2).list }, X::AdHoc, 'literal .list topic is read-only';
throws-like { $_ = 5 for (1,2).reverse }, X::AdHoc, 'literal .reverse topic is read-only';
throws-like { $_ = 5 for (1,2).pairs }, X::AdHoc, 'literal .pairs topic is read-only';
throws-like { $_ = 5 for (1,2).sort }, X::AdHoc, 'literal .sort topic is read-only';
throws-like { for (1,2).kv -> \k, \v { v = 5 } }, X::Assignment::RO,
    'sigilless value of a literal .kv is not assignable';
throws-like { for (1,2) -> \x { x = 5 } }, X::Assignment::RO,
    'sigilless parameter over a literal list is not assignable';
throws-like { for (1,2).pairs -> \p { p = 5 } }, X::Assignment::RO,
    'sigilless parameter over literal .pairs is not assignable';
{
    my @l := (1, 2);
    throws-like { $_ = 5 for @l.sort }, X::AdHoc, '.sort of a bound List is read-only';
}

# --- Containers must stay writable ---------------------------------------------
{
    my @a = 3, 1, 2;
    $_ = 5 for @a.sort;
    is-deeply @a, [5, 5, 5], '.sort of a mutable Array still aliases its elements';
}
{
    my @a = 1, 2;
    $_ = 5 for @a.values;
    is-deeply @a, [5, 5], '.values of a mutable Array still aliases its elements';
}
{
    my ($x, $y) = 1, 2;
    $_ = 9 for ($x, $y).list;
    is "$x $y", '9 9', 'a list of variables keeps its containers writable';
}
{
    my @a = 1, 2;
    for @a -> \x { x = 7 }
    is-deeply @a, [7, 7], 'sigilless parameter over an Array writes through';
}
{
    my %h = a => 1;
    for %h.kv -> \k, \v { v = 5 }
    is %h<a>, 5, 'sigilless value of %h.kv writes through';
}
{
    my @a = 1, 2;
    for @a.kv -> \k, \v { v = 5 }
    is-deeply @a, [5, 5], 'sigilless value of @a.kv writes through';
}
lives-ok { for (1,2).values -> $x { $x.say if False } }, 'reading the items is fine';
