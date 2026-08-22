use Test;

plan 6;

# A `my` declared in a grammar body (like a class body — see
# t/class-body-my-lexical-scope.t) is a lexical of that body: a token/rule
# method sees it, the same way an ordinary method sees a class-body static.
# mutsu's class-body-static injection (`inject_class_body_statics`) only ran
# on the two method-dispatch paths, never on grammar token/subrule dispatch,
# so a pattern that interpolates such a variable (`@array`/`$scalar`/`%hash`)
# silently resolved it to Nil at match time instead of its declared value —
# discovered via Cro::HTTP::Cookie's `token cookie-av:sym<samesite> { :i
# 'SameSite=' @same-site-opts }`, where `@same-site-opts` is a grammar-body
# `my @same-site-opts = SameSite.enums.values;`. Real `SameSite=Strict`
# cookies silently lost their `SameSite` attribute.

grammar GBMLS-Basic {
    my @opts = <Strict Lax None>;
    token TOP { :i 'Foo=' @opts }
}
ok GBMLS-Basic.parse('Foo=Strict'), 'a grammar-body my @array interpolates into a token pattern';
nok GBMLS-Basic.parse('Foo=Dog'), 'and a value outside the array correctly fails to match';

# The actual bug shape: a proto token where one `:sym<>` candidate's pattern
# ends in an array interpolation and a SIBLING candidate is a generic
# catch-all that calls a NAMED SUBRULE (not an inline quantified atom — see
# below). When both can match the same input, Rakudo's LTM prefers the
# array-interpolated candidate; before this fix mutsu always picked the
# catch-all because the array silently resolved to empty, so the two
# candidates' real match lengths were never actually tied.
my regex gbmls-path { <[\x1F..\xFF] - [;]>+ }

grammar GBMLS-Proto {
    my @opts = <Strict Lax None>;
    token TOP { <name> [';' ' '? <val> ]* }
    token name { <-[;]>+ }
    proto token val {*}
    token val:sym<known> { :i 'Foo=' @opts }
    token val:sym<other> { <gbmls-path> }
}
class GBMLS-Actions {
    method val:sym<known>($/) { make 'KNOWN' }
    method val:sym<other>($/) { make 'OTHER' }
}

my $m1 = GBMLS-Proto.parse('x; Foo=Strict', :actions(GBMLS-Actions.new));
is $m1<val>[0].made, 'KNOWN',
    'an array-interpolated candidate wins an LTM tie over a sibling subrule catch-all';

my $m2 = GBMLS-Proto.parse('x; Foo=Dog', :actions(GBMLS-Actions.new));
is $m2<val>[0].made, 'OTHER',
    'when the array-interpolated candidate genuinely cannot match, the catch-all still wins';

# Direct `:rule<name>` dispatch (bypassing the nested-subrule path above) must
# resolve the same lexical too. This raced `known` against `GBMLS-Proto`'s
# named-subrule `other` candidate, which used to lose to a separate LTM gap:
# `gbmls-path`'s subtracted character class (`<[\x1F..\xFF] - [;]>`) wrongly
# earned full greedy declarative-prefix credit. Rakudo cannot encode a set
# subtraction as one NFA edge, so such a class terminates the prefix; fixed in
# ADR-0046 Slice 4, so the strong form of this assertion is back.
my $m3 = GBMLS-Proto.parse('Foo=Strict', :rule<val>, :actions(GBMLS-Actions.new));
is $m3.made, 'KNOWN', 'direct :rule<name> dispatch also sees the grammar-body my @array';

# A grammar-body my is unbound outside the grammar, like a class-body my.
nok $::('opts').defined, 'the grammar-body my does not leak into the enclosing scope';

done-testing;
