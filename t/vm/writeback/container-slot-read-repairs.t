use Test;

# ADR-0039 slice 2: `@`/`%` reads compile to `GetLocal(slot)` for a plain user
# lexical, so container scoping is lexical rather than dynamic.
#
# Turning the read into a slot read exposed a family of store-side defects that
# the by-name read had been papering over: two paths that end up naming
# different containers under one name look fine as long as every read
# re-resolves the name. Each block below is one of those defects. They are
# collected here because each was found through the flip rather than through a
# feature, so no existing file covers them as a set.

plan 14;

# Repair 3 — a container declaration in EXPRESSION position must take the slot
# its own reads resolve to. `local_map` is monotonic, so a popped sibling
# block's `@a` slot stays reachable and the expression-position declaration
# stored into `env` alone while every read answered the sibling's slot.
{
    { my @a = 5, 7, 9 }
    (my @a).push: $_ for ^3;
    is @a.join(','), '0,1,2', 'expression-position @ declaration takes the read slot';
}
{
    { my %sib = :a(1) }
    (my %h)<k> = 'v';
    is %h.keys.join(','), 'k', 'expression-position % declaration takes the read slot';
}

# A SHAPED declaration in expression position keeps its declared shape. The
# `SetGlobal` route the expression path used to take never needed the marker the
# statement path emits; taking the slot route made it load-bearing.
{
    is (-> @a[3] { @a[1] })(my @b[3] = <a b c>), 'b',
        'a shaped expression-position declaration keeps its shape';
    is (my @c[3] = <p q r>).shape.join(','), '3',
        'and reports it back through the declaration expression';
}

# Repair 4 — the fast hash-element-assign path must resolve its target through
# the compiler-baked slot. `find_local_slot` is a `position` search, so with a
# same-named shadow (`code.locals == ["%h", "%h"]`) it nil'd and re-seeded the
# OUTER binding.
{
    my %h = :outer(1);
    {
        my %h;
        %h<k> = 'inner';
        is %h.keys.join(','), 'k', 'a shadowing hash element-assign stays in the shadow';
    }
    is %h.keys.join(','), 'outer', 'and leaves the outer hash untouched';
}

# Repair 6 — a mutating hyper must write its result THROUGH the target's
# container node. The old fallback searched `code.locals` by name for a slot to
# keep in sync, so under a same-named shadow it wrote the OUTER slot and `@r>>++`
# answered the unincremented `1 2 3`.
{
    my @r;
    {
        my @r = (1, 2, 3);
        @r>>++;
        is @r.join(','), '2,3,4', 'a shadowing @ hyper-postfix writes its own binding';
    }
    is @r.elems, 0, 'and leaves the outer array untouched';
}

# Repair 7 — the `is BagHash`/`SetHash`/`MixHash` trait handler re-syncs its
# slot after registering the name-keyed constraint. That registration re-tags
# `env`'s value through `Gc::make_mut`, which COPIES a node the slot shares, so
# `env` ended up on the tagged copy while the slot kept the untagged original.
{
    my %m is MixHash = a => 2, b => 1;
    %m<a>--;
    is %m.pairs.sort(*.key).map({ .key ~ '=' ~ .value }).join(','), 'a=1,b=1',
        'a MixHash declared with a trait stays coherent with its slot';
}
{
    my %b is BagHash = <a a b>;
    %b<a>--;
    is %b.pairs.sort(*.key).map({ .key ~ '=' ~ .value }).join(','), 'a=1,b=1',
        'and so does a BagHash';
}

# The container-identity rule the repairs generalise: a runtime helper that
# REBUILDS a variable's container copies the result into the existing backing
# node instead of dropping a fresh node into `env`. An element store reached
# through an accessor is the shape that exercises the by-identity alias sweep.
{
    class A { has @.seen; method bag() { @!seen } }
    my $a = A.new(seen => []);
    my @alias := $a.bag;
    $a.bag[1] = 'x';
    is @alias[1], 'x', 'an accessor element store reaches an aliased binding';
}
# ...and that in-place copy must not erase the destination's declared container
# metadata. An element-level rebuild does not carry the variable's element type,
# so overwriting the node wholesale dropped `my CSV::Field @f`'s `of` and the
# next call that wanted `(CSV::Field:D @fld)` no longer bound (Text::CSV's
# `10_base.t`).
{
    class F { has $.v }
    class R { has F @.fields; method f() { @!fields } }
    sub takes(F:D @fld) { @fld.elems }
    my $r = R.new(fields => [F.new(v => 1)]);
    $r.f[1] = F.new(v => 2);
    is takes($r.fields), 2, 'a typed array attribute keeps its element type across an element store';
    is $r.fields.of.^name, 'F', 'and still reports it through .of';
}

{
    class H { has %.seen; method bag() { %!seen } }
    my $h = H.new(seen => {});
    my %alias := $h.bag;
    $h.bag<c> = 3;
    is %alias<c>, 3, 'and the hash twin does too';
}
