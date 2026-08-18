use Test;

plan 11;

# Container identity (§3.1, todo/deep/hash-pointy-param-writeback-loses-object-hash-identity.md):
# once a hash local's slot has been promoted to a shared `ContainerRef` cell
# (e.g. by passing it into a `Mu $x` parameter -- the "scalar-container-share"
# promotion), a later whole-container reassignment through that cell must
# keep the hash's key/value-type identity and write through to every other
# holder of the same container, instead of silently demoting to a plain
# `Hash` and detaching from the original.

sub my-check(Mu $got) { 1 }

# 1. A call-boxed for-loop pointy-block param keeps `Hash[Any,Any]` identity
# and writes through to the outer object hash it is aliased to.
{
    my %h = "a".."c" Z=> 1..3;
    my %ao{Any} = %h;
    my %reseto{Any} = "a".."c" Z=> 100..102;
    for (%ao,) -> %a {
        my-check(%a);
        %a = %reseto;
        is %a.WHAT.raku, 'Hash[Any,Any]',
            'boxed loop param keeps object-hash identity after write-through';
        is-deeply %a.raku, %reseto.raku,
            'boxed loop param value matches the reassigned hash';
    }
    is-deeply %ao.raku, %reseto.raku,
        'outer object hash observes the write-through mutation';
}

# 2. The same shape for a value-typed (non-object) hash: `Hash[Cool]`.
{
    my Cool %c = a => 1, b => 2;
    my %alias := %c;
    my-check(%alias);
    %alias = (a => 10, b => 20);
    is %alias.WHAT.raku, 'Hash[Cool]',
        'boxed value-typed hash keeps its declared type after write-through';
    is-deeply %alias.raku, '(my Cool % = :a(10), :b(20))',
        'boxed value-typed hash value matches the reassigned hash';
    is-deeply %c.raku, %alias.raku,
        'aliased value-typed hash observes the write-through mutation';
}

# 3. A plain (untyped) hash stays plain -- unchanged behavior.
{
    my %p = a => 1, b => 2;
    my %alias := %p;
    my-check(%alias);
    %alias = (a => 10, b => 20);
    is %alias.WHAT.raku, 'Hash', 'boxed plain hash stays plain after write-through';
    is-deeply %alias.raku, %p.raku, 'aliased plain hash observes the write-through mutation';
}

# 4. The metaop-writeback shape from roast/S03-metaops/infix.t: a
# `<<[&metaop]>>` hyper-func-op writeback on a for-loop-aliased object hash.
{
    my &metaop = &[~=];
    my %a = "a".."c" Z=> 1..3;
    my %ao{Any} = %a;
    for (%ao,) -> %x {
        %x <<[&metaop]>> 3;
        is %x.WHAT.raku, 'Hash[Any,Any]',
            'metaop writeback keeps object-hash identity';
        is-deeply %x.raku, '(my Any %{Any} = :a("13"), :b("23"), :c("33"))',
            'metaop writeback value matches expected result';
    }
    is-deeply %ao.raku, '(my Any %{Any} = :a("13"), :b("23"), :c("33"))',
        'outer object hash observes the metaop writeback';
}
