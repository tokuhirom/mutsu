use Test;
plan 13;

# Basic two-signature multi dispatch
{
    multi sub si  (Str $s, Int $i)
                | (Int $i, Str $s) {
        "s:$s i:$i";
    }
    is si("a", 3), "s:a i:3", 'two sigs dispatch (Str, Int)';
    is si(3, "b"), "s:b i:3", 'two sigs dispatch (Int, Str)';
}

# Named parameters with a Num type constraint. raku-strict (verified against
# Rakudo v2026.06 with plain multi syntax, since rakudo does not parse the
# signature-alternates form used in this file): only a real Num matches; Rat
# and Int arguments are rejected at dispatch with X::Multi::NoMatch, NOT
# widened. The pre-2026-08 "numeric widening" this block used to pin was a
# mutsu-only dialect.
{
    multi sub foo (Int $i, Num :$n) {
        "$i $n";
    }
    is foo(4, :n(2.3e0)), '4 2.3', 'Num named param accepts Num value';
    throws-like { foo(4, :n(2.3)) }, X::Multi::NoMatch, 'Num named param rejects Rat value';
    throws-like { foo(4, :n(42)) }, X::Multi::NoMatch, 'Num named param rejects Int value';
}

# Three signatures with named params
{
    multi sub three  (Str $s, Int $i, Num :$n)
                   | (Int $i, Str :$s, Num :$n)
                   | (Num :$s, Int :$i, Str :$n) {
        "$s $i $n";
    }
    # Rat args bind to `Num` params HERE because the S13-era alternates syntax
    # translates `Num` to `Numeric` at the feature boundary (old-spec meaning;
    # roast/S06-signature/multiple-signatures.t depends on it). Outside the
    # alternates form, `Num` is raku-strict — see the foo() block above.
    is three('abc', 3, :n(2.3)), 'abc 3 2.3', 'three sigs dispatch (1)';
    is three(4, :s<x>, :n(2.3)), 'x 4 2.3', 'three sigs dispatch (2)';
    is three(:i(4), :s(0.2), :n('f')), '0.2 4 f', 'three sigs dispatch (3)';
}

# State variable sharing across signature alternates
{
    multi sub count  (Str $s, Int $i)
                | (Int $i, Str $s) {
        state $x = 0;
        ++$x;
    }
    is count("a", 3), 1, 'state var init in multi with two sigs';
    is count("a", 2), 2, 'state var persists';
    is count(2, 'a'), 3, 'state var shared across alternates';
}

# Validation: different variable sets rejected
throws-like q[ multi sub x ($x, $y) | ($x, $y, $z) { 1 }], Exception,
    'reject different variable count';
throws-like q[ multi sub x ($x, $y) | ($x, @y) { 1 }], Exception,
    'reject different sigils';
