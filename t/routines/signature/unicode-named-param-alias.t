use Test;

# A named parameter's external alias is an identifier like any other, so it may
# start with any Unicode alphabetic character — `:ν(:$!nu) = 1` is how
# Statistics::Distributions (and Math::GameTheory through it) writes a BUILD
# submethod.
#
# mutsu supports Unicode identifiers everywhere else (variables, subs, classes,
# methods), but the guard that recognises `:name(...)` as an alias rather than a
# type constraint tested the first BYTE for `is_ascii_alphabetic`. A UTF-8 lead
# byte such as 0xCE is not ASCII-alphabetic, so the alias fell into the
# type-constraint/coercion path instead — which parses no trailing default, so
# the defaulted form failed outright and the undefaulted form silently lost the
# alias name:
#
#     sub f(:ν($nu) = 1) { }    # "Confused. ... expected ')'"
#     sub f(:ν($nu))    { }     # parsed, then "Unexpected named argument 'ν'"
#
# That is the same failure the uppercase case (`:ASTART($a) = 0`) already had,
# reached by a different route; the guard now asks the identifier oracle
# `is_raku_identifier_start` about the first CHARACTER.

plan 12;

# The distribution's own shape: a Unicode alias onto a private attribute, with
# a default.
{
    class C {
        has $!nu;
        submethod BUILD(:ν(:$!nu) = 1) { }
        method nu { $!nu }
    }
    is C.new.nu, 1, 'a Unicode alias with a default takes the default';
    is C.new(:ν(7)).nu, 7, 'and binds when the caller supplies it';
}

# Reduced to a plain sub, both levels of the alias and both with and without a
# default.
{
    sub two-level(:ν(:$nu) = 1) { $nu }
    is two-level(), 1, 'two-level Unicode alias, defaulted';
    is two-level(:ν(5)), 5, 'two-level Unicode alias, supplied';

    sub one-level(:ν($nu) = 1) { $nu }
    is one-level(), 1, 'single-level Unicode alias, defaulted';
    is one-level(:ν(5)), 5, 'single-level Unicode alias, supplied';

    sub no-default(:ν($nu)) { $nu }
    is no-default(:ν(5)), 5, 'single-level Unicode alias with no default';
}

# Not specific to Greek: any Unicode alphabetic start works.
{
    sub sigma(:σ($s)) { $s }
    is sigma(:σ(2)), 2, 'a sigma alias binds';

    sub cjk(:名前($s)) { $s }
    is cjk(:名前(3)), 3, 'a CJK alias binds';
}

# The spellings that already worked must keep working — the guard got wider,
# not different.
{
    sub ascii(:a-b($s)) { $s }
    is ascii(:a-b(2)), 2, 'a hyphenated ASCII alias still binds';

    sub upper(:ASTART($a) = 0) { $a }
    is upper(:ASTART(9)), 9, 'an uppercase alias with a default still binds';
}

# And a real type constraint on a named parameter must still be a type
# constraint, not be mistaken for an alias.
{
    sub typed(Int :$x = 3) { $x }
    is typed(:x(8)), 8, 'a type-constrained named param is unaffected';
}
