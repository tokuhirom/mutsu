use v6;
use Test;

# §1.5 S16 pin (docs/lexical-scope-slot-campaign.md): ApplyVarTrait must
# resolve the DECLARED variable's slot from the compile-time-baked slot, not
# a by-name `position` search. Under MUTSU_SHADOW_SLOTS=1 the by-name search
# hits the OUTER same-named slot, so `my @a is default(42)` inside a block
# tagged the outer array and clobbered env with it (the inner shadow then
# read the outer contents plus the inner default). Both toggle modes must
# pass this file. Regressed shape: roast/integration/advent2013-day12.t.

plan 8;

my @a = 7, 8, 9;
my %h = x => 9;

{
    my @a is default(42) = ^10;
    is @a[1], 1, 'inner shadowed array with is default reads its own elements';
    @a[3]:delete;
    is-deeply @a[2, 3, 4], (2, 42, 4), 'deleted element returns the inner default';
    is-deeply (@a[2, 3, 4]:exists), (True, False, True), ':exists sees the deleted hole';
}

{
    my %h is default(42) = a => 1, b => 2;
    is %h<a>, 1, 'inner shadowed hash with is default reads its own elements';
    is %h<c>, 42, 'missing key returns the inner default';
}

is-deeply @a, [7, 8, 9], 'outer array is untouched after the shadow block';
is-deeply %h, {x => 9}, 'outer hash is untouched after the shadow block';

{
    my %h is Bag = <a b b>;
    is %h<b>, 2, 'QuantHash trait applies to the inner shadow, not the outer hash';
}
