use Test;

# DSL::Entity::Metadata (ecosystem) built a >1000-entry Hash via
# `@keys.map(*.lc) Z=> @values`; the VM's `Z` meta-op unconditionally
# clamped its result to 1000 pairs regardless of how long the (finite,
# eager) operands actually were, silently dropping most of the data.

plan 7;

my @a = (1 .. 2000);
my @b = (1 .. 2000);
is (@a Z=> @b).elems, 2000, 'Z=> on two large finite arrays keeps every pair';
is (@a Z @b).elems, 2000, 'plain Z on two large finite arrays keeps every pair';

is (1 .. 2000 Z 1 .. 2000).elems, 2000, 'Z on two large finite ranges keeps every pair';
is ((1 .. 500) Z (1 .. 500) Z (1 .. 500)).elems, 500,
    'n-ary Z on large finite operands keeps every pair';

# A genuinely infinite range must still behave: bounded by the finite
# side when mixed, and still refuse .elems when both sides are infinite.
is (1 .. * Z 1 .. 2000).elems, 2000,
    'Z between an infinite range and a large finite one is bounded by the finite side';
is (1 .. 2000 Z 1 .. *).elems, 2000,
    'Z between a large finite range and an infinite one is bounded by the finite side';

dies-ok { (1 .. * Z 1 .. *).elems }, 'Z of two infinite ranges is still lazy (cannot .elems)';
