use v6;
use Test;

# A plain Int compared against an enum VALUE (not the enum type object) must
# compare by its underlying numeric value, matching Raku's `Enumeration`
# ACCEPTS semantics — this even crosses enum types (`Red ~~ Apple` is True
# when both underlying values are 0). Previously `pure_smart_match` had no
# arm at all for a bare `Int`/`Enum` pair, so it fell through to the
# interpreter's generic string-equality fallback: the stringified Int ("0")
# compared against the enum key's own name ("CBOR_UInt") — always False.
#
# This is a very common idiom for binary-protocol parsers (CBOR::Simple's
# `cbor-diagnostic`/`cbor-decode`): `given ($byte +& CBOR_MajorType_Mask) {
# when CBOR_UInt { ... } }`.

plan 14;

enum Color (Red => 0, Green => 5, Blue => 10);

ok 0 ~~ Red, 'Int 0 ~~ enum value Red (both 0)';
nok 0 ~~ Green, 'Int 0 !~~ enum value Green';
ok 5 ~~ Green, 'Int 5 ~~ enum value Green';
ok 10 ~~ Blue, 'Int 10 ~~ enum value Blue';
nok 3 ~~ Red, 'Int with no matching enum value smartmatches False';

# Symmetric: enum value on the LHS.
ok Red ~~ 0, 'enum value Red ~~ Int 0';
nok Green ~~ 0, 'enum value Green !~~ Int 0';

# Cross-enum-type value equality (verified against real `raku`).
enum Fruit (Apple => 0, Banana => 1);
ok Red ~~ Apple, 'Red ~~ Apple (different enum types, same underlying value)';

# given/when — the actual compiled path CBOR::Simple's dispatch uses.
my $major = 0;
my $result;
given $major {
    when Red   { $result = 'red' }
    when Green { $result = 'green' }
    default    { $result = 'none' }
}
is $result, 'red', 'given/when dispatches on enum value smartmatch';

# Junction autothreading against an enum value still works (routed through
# the VM's own junction handling, unaffected by the direct-comparison fix).
ok (0|5) ~~ Red, 'Junction ~~ enum value autothreads (Any branch)';
nok (1|2) ~~ Red, 'Junction ~~ enum value autothreads, all-false case';

# Enum TYPE object smartmatch (the pre-existing mechanism) must still work.
ok Red ~~ Color, 'enum value ~~ its own enum type object';
ok Green ~~ Color, 'another enum value ~~ its own enum type object';

# Numeric `==`/`<=`/`>=` comparisons against an enum value (a SEPARATE code
# path from smartmatch — `coerce_infix_operand_numeric`, the shared bridge
# for `==`/`!=`/`<`/`>`/`<=`/`>=`/`<=>` and arithmetic). Exercises a value
# outside f64's exact-integer range, which the generic same-variant fallback
# (numify-both-to-f64) would silently corrupt.
enum BigTag (MaxU64 => 18446744073709551615);
ok 18446744073709551615 == MaxU64, 'large Int == enum value (exact BigInt precision)';
