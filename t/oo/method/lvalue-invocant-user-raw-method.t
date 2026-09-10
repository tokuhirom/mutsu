use v6.e.PREVIEW;
use MONKEY-TYPING;
use Test;

# ADR-0067, the E6 producer, user-declared half: the container an `is rw`
# attribute accessor hands an lvalue invocant is consumed by a *user* method
# that binds parameter zero raw, not only by the native `.snitch`.
#
# This file also exercises the user side of the runtime gate on
# `OpCode::MarkLvalueInvocantRefContext` (`Registry::any_raw_invocant_method`),
# which the sibling file `t/lvalue-invocant-attribute-accessor-container.t`
# cannot: that one declares no raw invocant at all.
#
# Byte-identical under `raku` and `mutsu`.

plan 8;

augment class Any {
    # Raw invocant AND rw-capable -- the contract's two halves, all three
    # rw-capable spellings.
    method mutsuE6Raw(\S:) is raw { S }
    method mutsuE6Rw(\S:) is rw { S }
    method mutsuE6ReturnRw(\S:) { return-rw S }
    # Raw invocant, but the body returns a value rather than a location.
    method mutsuE6RawValue(\S:) is raw { 42 }
    # Raw invocant, NOT rw-capable -- raku: "Cannot modify an immutable Int".
    method mutsuE6NotRw(\S:) { S }
    # rw-capable, but the invocant is NOT raw -- raku: "Cannot assign to a
    # readonly variable or a value".
    method mutsuE6NotRawInv(Any:D $s:) is raw { $s }
}

class Holder { has $.v is rw }

{
    my $c = Holder.new(v => 42);
    $c.v.mutsuE6Raw = 9;
    is $c.v, 9, '`is raw` with a raw invocant writes through the attribute';
}

{
    my $c = Holder.new(v => 42);
    $c.v.mutsuE6Rw = 9;
    is $c.v, 9, '`is rw` with a raw invocant writes through the attribute';
}

{
    my $c = Holder.new(v => 42);
    $c.v.mutsuE6ReturnRw = 9;
    is $c.v, 9, '`return-rw` with a raw invocant writes through the attribute';
}

{
    # An ordinary rvalue call must still hand back a plain value.
    my $c = Holder.new(v => 42);
    my $r = $c.v.mutsuE6Raw;
    $r = 100;
    is $c.v, 42, 'an rvalue call through the same method is still a copy';
}

{
    my $c = Holder.new(v => 42);
    dies-ok { $c.v.mutsuE6RawValue = 9 },
        'a raw-invocant body returning a value still refuses';
    is $c.v, 42, 'and the attribute is unchanged';
}

{
    my $c = Holder.new(v => 42);
    dies-ok { $c.v.mutsuE6NotRw = 9 },
        'a raw invocant that is not rw-capable still refuses';
}

{
    my $c = Holder.new(v => 42);
    dies-ok { $c.v.mutsuE6NotRawInv = 9 },
        'an rw-capable method whose invocant is not raw still refuses';
}

done-testing;
