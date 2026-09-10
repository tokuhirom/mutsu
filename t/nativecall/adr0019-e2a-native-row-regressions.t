use Test;
plan 17;

# ADR-0019 Phase E box E2a (`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`,
# `todo/deep/adr0019-e2-e4-resolver-core.md` decision 2): pins for the regression
# cases the design doc names as the exact failure modes the reverted 2026-08-04
# handler-ID attempt hit -- a type object dispatching a catalog method, a user
# `is Array` subclass's storage delegation, `Map`'s Hash-owned methods, a
# gather-based `Seq` vs an eager `Array`, `Failure`'s must-not-silently-dispatch
# contract, `Rat`/`FatRat`, and allomorph (`IntStr`) dispatch. `native_method_row`
# rows are not load-bearing yet (E2a is recognition-metadata only), so this file
# guards today's real dispatch behavior -- the contract E4b's cutover must not
# break once native rows start driving real decisions.

# 1) Type object receiving a catalog method (Str.gist / Int.raku): the pure
# arity cascade must still recognize the call when the receiver is a type
# object, not just a defined instance (E2a's TYPE_OBJECT_OK regression case).
is Str.gist, '(Str)', 'Str type object .gist';
is Int.raku, 'Int', 'Int type object .raku';

# 2) User `is Array` subclass: native Positional methods must delegate to the
# backing storage attribute, not the Instance itself.
class E2aArraySub is Array { }
my E2aArraySub $arr .= new;
$arr.push(1, 2, 3);
is $arr.elems, 3, 'Array-subclass instance .elems delegates to storage';
is $arr.join(','), '1,2,3', 'Array-subclass instance .join delegates to storage';

# 3) Map: both its own methods and the Hash-owned methods it shares must
# resolve on a Map receiver (not just on a plain Hash).
my %m := Map.new('a' => 1, 'b' => 2);
is %m.elems, 2, 'Map .elems (Hash-owned)';
is %m.keys.sort.join(','), 'a,b', 'Map .keys (Hash-owned)';

# 4) gather-based Seq must stay a Seq (lazy), distinct from an eager Array --
# the native row catalog's `List`/`Array` owner split must not collapse them.
my $seq = gather { take 1; take 2; take 3 };
is $seq.WHAT.gist, '(Seq)', 'gather block produces a Seq, not an Array';
is $seq.elems, 3, 'Seq .elems forces and counts';

# 5) Failure must not silently dispatch through the native cascade: an
# unhandled method call explodes instead of returning a bogus native result,
# but the handled-Failure operations (.Bool here) mark it as handled -- both
# must keep working the same way once a `Failure` row is added and marked
# SPECIAL in E2b.
my $f1 = Failure.new('adr0019-e2a-probe');
is $f1.Bool, False, 'Failure .Bool marks it handled, does not throw';
my $f2 = Failure.new('adr0019-e2a-probe');
dies-ok { $f2.no-such-method-xyz }, 'unhandled Failure method call explodes';

# 6) Rat / FatRat: both share the `Rat` native row owner but must keep
# their own type identity.
my $rat = <1/3>;
is $rat.WHAT.gist, '(Rat)', 'Rat literal keeps Rat type';
is $rat.Str, '0.333333', 'Rat .Str native coercion';
my $fatrat = FatRat.new(1, 3);
is $fatrat.WHAT.gist, '(FatRat)', 'FatRat.new keeps FatRat type';
is $fatrat.Str, '0.333333', 'FatRat .Str native coercion';

# 7) Allomorph (IntStr): dispatch must resolve both the Int and Str facets
# through the same native cascade.
my $allomorph = <5>;
is $allomorph.WHAT.gist, '(IntStr)', 'angle-bracket literal is an IntStr allomorph';
is $allomorph + 1, 6, 'IntStr allomorph dispatches its Int facet';
is $allomorph ~ 'x', '5x', 'IntStr allomorph dispatches its Str facet';
