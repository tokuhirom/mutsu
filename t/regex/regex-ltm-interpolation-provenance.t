use v6;
use Test;

plan 20;

# ADR-0046 Slice 1: array- and regex-object-valued regex interpolation forms
# (`@name`, `<@name>`, `@(...)`, `@$ref`, `<$var>` holding a Regex) terminate
# the declarative LTM prefix unconditionally, just like a non-constant
# `$`-scalar interpolation already did before this slice (ADR-0022 Slice 5).
# See docs/adr/0046-proto-token-ltm-shares-one-ranking-mechanism.md §2.1 for
# the full probe table, verified against `raku` on 2026-08-20.
#
# Subject "StrictX"; branch 2 of each alternation is the fixed literal 'St'
# (declarative prefix length 2). If the leading interpolation wrongly
# participated in branch 1's declarative prefix, branch 1's prefix would be
# 7 ('Foo=' analogue is absent here, so really just "the whole interpolated
# alternative plus 'X'") and it would win LTM, matching "StrictX" instead of
# the correct "St".

# Probe I: bare `@name` interpolation from a `my` array.
{
    my @opts = <Strict Lax None>;
    ok "StrictX" ~~ / @opts 'X' | 'St' /, 'probe I: matches';
    is ~$/, 'St', 'probe I: @opts (my array) terminates the declarative prefix';
}

# Probe J: bare `@name` interpolation from a `constant` array -- unlike the
# `$`-scalar case, there is no constant-vs-non-constant exemption for `@`.
{
    my constant @copts = <Strict Lax None>;
    ok "StrictX" ~~ / @copts 'X' | 'St' /, 'probe J: matches';
    is ~$/, 'St', 'probe J: @copts (constant array) terminates too -- no @ exemption';
}

# Probe K: the `<@var>` assertion form terminates too.
{
    my @opts = <Strict Lax None>;
    ok "StrictX" ~~ / <@opts> 'X' | 'St' /, 'probe K: matches';
    is ~$/, 'St', 'probe K: <@opts> assertion form terminates too';
}

# Probe M: an array of Regex objects -- element type is irrelevant.
{
    my @ropts = (rx/Strict/, rx/Lax/, rx/None/);
    ok "StrictX" ~~ / @ropts 'X' | 'St' /, 'probe M: matches';
    is ~$/, 'St', 'probe M: array of Regex objects terminates too';
}

# Probe L: negative control -- a hand-written literal alternation (no
# interpolation at all) still fully participates in LTM ranking.
{
    ok "StrictX" ~~ / [ 'Strict' | 'Lax' | 'None' ] 'X' | 'St' /,
        'probe L: matches';
    is ~$/, 'StrictX',
        'probe L: hand-written literal alternation still participates (negative control)';
}

# Probe Q: the `@(...)` contextualizer form terminates too.
{
    ok "StrictX" ~~ / @(<Strict Lax>) 'X' | 'St' /, 'probe Q: matches';
    is ~$/, 'St', 'probe Q: @(...) contextualizer terminates too';
}

# Probe R: the `@$ref` scalar-deref-as-array form terminates too.
{
    my @opts = <Strict Lax None>;
    my $ref := @opts;
    ok "StrictX" ~~ / @$ref 'X' | 'St' /, 'probe R: matches';
    is ~$/, 'St', 'probe R: @$ref deref form terminates too';
}

# Probe S: the `<$var>` regex-value reroute terminates too (a scalar holding
# a Regex object, spliced via `<$var>` syntax rather than array alternation).
{
    my $rx = rx/Strict/;
    ok "StrictX" ~~ / <$rx> 'X' | 'St' /, 'probe S: matches';
    is ~$/, 'St', 'probe S: <$rx> regex-value form terminates too';
}

# ---------------------------------------------------------------------------
# ADR-0046 Slice 2: the *bound* interpolator (`interpolate_bound_regex_scalars`),
# which renders grammar/token bodies, must mark its substitutions the same way.
# Before this slice only the general-case `interpolate_regex_scalars` did, so a
# `$var` inside a token body stayed indistinguishable from a hand-written
# literal and wrongly extended the candidate's declarative LTM prefix.
# Expectations verified against `raku`; see ADR-0046 §2.2's `scalar` row.

class LTMActions {
    method val:sym<known>($/) { make 'KNOWN' }
    method val:sym<other>($/) { make 'OTHER' }
}

# Probe T: a `my` scalar interpolated into a token body terminates the prefix,
# so `known`'s prefix stops at 4 ('Foo=') and `other`'s 10 wins outright.
{
    grammar BodyScalar {
        my $opt = 'Strict';
        proto token val {*}
        token val:sym<known> { 'Foo=' $opt }
        token val:sym<other> { <-[;]>+ }
    }
    my $m = BodyScalar.parse('Foo=Strict', :rule<val>, :actions(LTMActions.new));
    ok $m, 'probe T: matches';
    is $m.made, 'OTHER',
        'probe T: a token body $var interpolation terminates the declarative prefix';
}

# Probe U: negative control -- a genuine `constant` IS inlined at compile time
# by Rakudo, so it keeps participating and `known` (prefix 10, declared first)
# wins the tie. This is the `$`-scalar exemption ADR-0022 Slice 5 established,
# which has no `@`-array analogue (probe J above).
{
    grammar BodyConstant {
        my constant $copt = 'Strict';
        proto token val {*}
        token val:sym<known> { 'Foo=' $copt }
        token val:sym<other> { <-[;]>+ }
    }
    my $m = BodyConstant.parse('Foo=Strict', :rule<val>, :actions(LTMActions.new));
    ok $m, 'probe U: matches';
    is $m.made, 'KNOWN',
        'probe U: a token body `constant` $var still participates (negative control)';
}
