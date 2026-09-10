use Test;

# The read-modify-write half of `t/dot-twigil-accessor-readonly.t`.
#
# `$.attr` inside a method is `self.attr` ITEMIZED. For a non-`rw` scalar
# accessor the accessor hands back a bare value, so the itemization is a fresh
# throwaway `Scalar` -- and a read-modify-write (`$.x *= 2`, `$.x++`,
# `$.x //= 9`) assigns into that throwaway. The expression still evaluates to
# the computed value, the attribute is unchanged, and NOTHING is thrown. Only
# the simple `$.x = v` is compiled without the itemize wrapper, so it alone hits
# the raw accessor return and dies (pinned by the sibling file).
#
# mutsu got this wrong in both directions at once, in different statement
# positions: `$.x *= 2` in expression position and `$.x++` anywhere silently
# MUTATED the attribute, while `$.x *= 2` as a bare statement and `$.x //= 9`
# both REFUSED with "method 'x' is not rw". Every row below was measured against
# raku v2026.07.

plan 17;

class RO {
    has $.x = 5;
    has $.s = "a";
    has @.a = (1, 2);

    method expr-paren   { my $r = ($.x *= 2); "$r/$!x" }
    method expr-bare    { my $r = $.x *= 2;   "$r/$!x" }
    method stmt-paren   { ($.x *= 2); $!x }
    method stmt-plain   { $.x *= 2; $!x }
    method stmt-concat  { $.s ~= "z"; $!s }
    method stmt-min     { $.x min= 1; $!x }
    method stmt-defor   { $.x //= 9; $!x }
    method stmt-orelse  { $.x orelse= 9; $!x }
    method post-inc     { my $r = $.x++; "$r/$!x" }
    method pre-inc      { my $r = ++$.x; "$r/$!x" }
    method post-dec     { my $r = $.x--; "$r/$!x" }
    method pre-dec      { my $r = --$.x; "$r/$!x" }
    method twice        { $.x++; $.x++; $!x }
    method arr-compound { @.a[0] += 10; @!a }
}

class RW {
    has $.y is rw = 5;
    method compound { $.y *= 2; $!y }
    method inc      { $.y++; $!y }
}

is RO.new.expr-paren,  '10/5', '($.x *= 2) yields the computed value, attribute untouched';
is RO.new.expr-bare,   '10/5', 'the unparenthesized expression form agrees';
is RO.new.stmt-paren,  5,      'a parenthesized statement leaves the attribute alone';
is RO.new.stmt-plain,  5,      'and so does the bare statement form';
is RO.new.stmt-concat, 'a',    '~= on a non-rw Str accessor is a no-op';
is RO.new.stmt-min,    5,      'min= is a no-op';
is RO.new.stmt-defor,  5,      '//= is a no-op';
is RO.new.stmt-orelse, 5,      'orelse= is a no-op';

is RO.new.post-inc, '5/5', '$.x++ yields the old value and leaves the attribute alone';
is RO.new.pre-inc,  '6/5', '++$.x yields the new value and leaves the attribute alone';
is RO.new.post-dec, '5/5', '$.x-- likewise';
is RO.new.pre-dec,  '4/5', '--$.x likewise';
is RO.new.twice,    5,     'two increments in a row still leave it at 5';

# A container accessor is not read-only in this sense: `@.a` hands back the
# Array itself, so an element compound assign is an ordinary element store.
is RO.new.arr-compound, [11, 2], '@.a[0] += 10 succeeds without is rw';

# `is rw` is unaffected: the accessor returns the attribute's real container,
# itemization is the identity, and the RMW genuinely mutates.
is RW.new.compound, 10, '$.y *= 2 mutates an is-rw attribute';
is RW.new.inc,       6, '$.y++ mutates an is-rw attribute';

# A private twigil RMW is always a real write.
class Priv { has $.z = 5; method bump { $!z *= 2; $!z } }
is Priv.new.bump, 10, '$!z *= 2 is unaffected';
