use v6;
use Test;

plan 35;

# ADR-0046 Decision 1: proto-token candidates are ranked by the ONE LTM ranking
# primitive (`ltm_branch_rank_key`) at every call site, i.e. by
# `(declarative prefix length desc, litlen desc, declaration order asc)`.
#
# The first half covers ADR-0046 §2.2 for *mechanism 1* -- the
# `parse(:rule<...>)` / outermost-proto entry point
# (`eval_token_call_values_at`). Before Slice 3 that site ranked on declarative
# prefix length alone, with no `litlen` tie-break. The second half covers
# *mechanism 3*, the nested `<name>` dispatch every real grammar goes through,
# which before Slice 4 did no declarative measurement at all: it matched every
# candidate for real and sorted by actual match end.
# All expectations were verified against `raku` first.

class LTMActions {
    method val:sym<known>($/) { make 'KNOWN' }
    method val:sym<other>($/) { make 'OTHER' }
}

# Row `code`: a plain `{ }` block is the oldest LTM prefix stopper mutsu
# implements (ADR-0009), so `known`'s prefix stops at 4 and `other`'s 10 wins.
{
    grammar GCode {
        proto token val {*}
        token val:sym<known> { 'Foo=' {} 'Strict' }
        token val:sym<other> { <-[;]>+ }
    }
    my $m = GCode.parse('Foo=Strict', :rule<val>, :actions(LTMActions.new));
    ok $m, 'code row: matches';
    is $m.made, 'OTHER', 'code row: a plain {} block terminates the declarative prefix';
}

# Row `ws`: `<.ws>` is a fate/terminate atom in Rakudo's NFA, same outcome.
{
    grammar GWs {
        proto token val {*}
        token val:sym<known> { 'Foo=' <.ws> 'Strict' }
        token val:sym<other> { <-[;]>+ }
    }
    my $m = GWs.parse('Foo=Strict', :rule<val>, :actions(LTMActions.new));
    ok $m, 'ws row: matches';
    is $m.made, 'OTHER', 'ws row: <.ws> terminates the declarative prefix';
}

# Row `litlen`: both candidates measure a declarative prefix of 3 on "abc", so
# the winner is decided by the ADR-0022 §4.3 `litlen` tie-break -- the literal
# 'abc' has litlen 3, the char-class walk `\w\w\w` has litlen 0. Declaration
# order deliberately favours the LOSER here, so a missing tie-break shows up.
{
    grammar GLit {
        proto token v {*}
        token v:sym<cc>  { \w\w\w }
        token v:sym<lit> { 'abc' }
    }
    class LitActions {
        method v:sym<lit>($/) { make 'LIT' }
        method v:sym<cc>($/)  { make 'CC' }
    }
    my $m = GLit.parse('abc', :rule<v>, :actions(LitActions.new));
    ok $m, 'litlen row: matches';
    is $m.made, 'LIT',
        'litlen row: equal prefixes are broken by litlen, not declaration order';
}

# A `:my` declarator is zero-width and transparent to LTM measurement: it must
# neither consume, nor terminate the prefix, nor be *executed* while measuring
# (ADR-0009). Both candidates therefore tie at prefix 10 and declaration order
# decides -- which means reversing the declarations flips the winner.
{
    grammar GMyFirst {
        proto token val {*}
        token val:sym<known> { :my $z = 1; 'Foo=Strict' }
        token val:sym<other> { <-[;]>+ }
    }
    my $m = GMyFirst.parse('Foo=Strict', :rule<val>, :actions(LTMActions.new));
    is $m.made, 'KNOWN', ':my is transparent: declared first, wins the prefix tie';

    grammar GMyLast {
        proto token val {*}
        token val:sym<other> { <-[;]>+ }
        token val:sym<known> { :my $z = 1; 'Foo=Strict' }
    }
    my $m2 = GMyLast.parse('Foo=Strict', :rule<val>, :actions(LTMActions.new));
    is $m2.made, 'OTHER', ':my is transparent: declared last, loses the prefix tie';
}

# ===========================================================================
# ADR-0046 Slice 4: mechanism 3 -- nested `<name>` proto dispatch -- is ranked
# by the same measurement instead of "match every candidate for real, keep the
# longest end". §2.2's rows again, this time reached through `token TOP { <val> }`.

sub nested($g, $subject = 'Foo=Strict') {
    my $m = $g.parse($subject, :actions(LTMActions.new));
    $m ?? ($m<val>.made // 'NOMADE') !! 'NOMATCH';
}

{
    grammar NCode {
        token TOP { <val> }
        proto token val {*}
        token val:sym<known> { 'Foo=' {} 'Strict' }
        token val:sym<other> { <-[;]>+ }
    }
    is nested(NCode), 'OTHER', 'nested: a plain {} block terminates the declarative prefix';
}

{
    grammar NWs {
        token TOP { <val> }
        proto token val {*}
        token val:sym<known> { 'Foo=' <.ws> 'Strict' }
        token val:sym<other> { <-[;]>+ }
    }
    is nested(NWs), 'OTHER', 'nested: <.ws> terminates the declarative prefix';
}

{
    grammar NScalar {
        my $opt = 'Strict';
        token TOP { <val> }
        proto token val {*}
        token val:sym<known> { 'Foo=' $opt }
        token val:sym<other> { <-[;]>+ }
    }
    is nested(NScalar), 'OTHER', 'nested: a token-body $var interpolation terminates it';
}

{
    grammar NArray {
        my @opts = <Strict Lax None>;
        token TOP { <val> }
        proto token val {*}
        token val:sym<known> { 'Foo=' @opts }
        token val:sym<other> { <-[;]>+ }
    }
    is nested(NArray), 'OTHER', 'nested: an array interpolation terminates it';
}

{
    grammar NLit {
        token TOP { <v> }
        proto token v {*}
        token v:sym<cc>  { \w\w\w }
        token v:sym<lit> { 'abc' }
    }
    class NLitActions {
        method v:sym<lit>($/) { make 'LIT' }
        method v:sym<cc>($/)  { make 'CC' }
    }
    my $m = NLit.parse('abc', :actions(NLitActions.new));
    is $m<v>.made, 'LIT', 'nested: equal prefixes are broken by litlen';
}

# ADR-0046 §2.3: ranking is a MEASUREMENT (ADR-0009: it never executes), and
# only the winner is matched -- so a losing candidate's `{ }` block must not run.
{
    my @ran;
    grammar NSideEffect {
        proto token v {*}
        token v:sym<x> { 'ab' { @ran.push('x') } 'c' }
        token v:sym<y> { 'abc' { @ran.push('y') } }
        token TOP { <v> }
    }
    NSideEffect.parse('abc');
    is @ran.join(','), 'y', 'nested: a losing proto candidate never runs its code block';
}

# The ADR's headline repro: needs Slice 1 (so `@opts` stops `known`'s prefix at
# 4) AND Slice 4 (so the nested `<val>` dispatch ranks by prefix at all).
{
    grammar Headline {
        my @opts = <Strict Lax None>;
        token TOP { <name> [';' ' '? <val> ]* }
        token name { <-[;]>+ }
        proto token val {*}
        token val:sym<known> { :i 'Foo=' @opts }
        token val:sym<other> { <-[;]>+ }
    }
    my $m = Headline.parse('x; Foo=Strict', :actions(LTMActions.new));
    is $m<val>[0].made, 'OTHER', 'headline repro: array interpolation loses to the catch-all';
}

# `<sym>` is a NAMED CAPTURE of the literal in Rakudo, not a bare literal
# splice. So it exposes `$<sym>`, and -- since a capture group ends the
# leading-literal region (ADR-0022 §4.3) -- it earns no litlen tie-break credit,
# unlike an equivalent hand-written literal.
{
    grammar SymCap {
        proto token v {*}
        token v:sym<ab> { <sym> 'X' }
    }
    my $m = SymCap.parse('abX', :rule<v>);
    ok $m, '<sym> matches';
    is ~($m<sym> // ''), 'ab', '<sym> exposes the $<sym> capture';
}
{
    class SymActs {
        method v:sym<cc>($/) { make 'CC' }
        method v:sym<ab>($/) { make 'AB' }
    }
    grammar SymLtm {
        proto token v {*}
        token v:sym<cc> { <-[/]>+ }
        token v:sym<ab> { <sym> }
    }
    is SymLtm.parse('ab', :rule<v>, :actions(SymActs.new)).made, 'CC',
        '<sym> earns no litlen credit, so the first-declared candidate keeps the tie';
    grammar PlainLtm {
        proto token v {*}
        token v:sym<cc> { <-[/]>+ }
        token v:sym<ab> { 'ab' }
    }
    is PlainLtm.parse('ab', :rule<v>, :actions(SymActs.new)).made, 'AB',
        'a hand-written literal DOES earn litlen credit (negative control)';
}

# A character class written with set SUBTRACTION cannot be one NFA edge in
# Rakudo, so it terminates the declarative prefix -- while every
# subtraction-free class participates normally. The rule is about the written
# structure, not the resulting character set, and not about the quantifier.
{
    ok "Foobar" ~~ / <[\x1F..\xFF] - [;]>+ | 'Foo' /, 'subtracted class: matches';
    is ~$/, 'Foo', 'a subtracted char class terminates the declarative prefix';

    ok "Foobar" ~~ / <-[;]>+ | 'Foo' /, 'plain negated class: matches';
    is ~$/, 'Foobar', 'a subtraction-free negated class participates (control)';

    ok "Foobar" ~~ / <+alpha - [q]>+ | 'Foo' /, 'subtracted named class: matches';
    is ~$/, 'Foo', 'a subtraction on a named class terminates too';

    ok "Foobar" ~~ / <+alpha>+ | 'Foo' /, 'plain named class: matches';
    is ~$/, 'Foobar', 'a subtraction-free named class participates (control)';

    ok "Foobar" ~~ / <-[;] - [q]>+ | 'Foo' /, 'all-negative multi-part class: matches';
    is ~$/, 'Foo',
        'the subtraction counts even when the result collapses to a plain negated set';

    ok "Foobar" ~~ / <[\x1F..\xFF] - [;]> \w* | 'Foo' /, 'unquantified subtracted class: matches';
    is ~$/, 'Foo', 'it is the subtraction, not the quantifier, that terminates';
}

# Reached through a named subrule the rule is unchanged -- the subrule is
# transparent, it is the class inside it that decides.
{
    my regex catchall { <[\x1F..\xFF] - [;]>+ }
    ok "Foobar" ~~ / <catchall> | 'Foo' /, 'subtracted class behind a subrule: matches';
    is ~$/, 'Foo', 'a subrule wrapping a subtracted class terminates the prefix too';

    my regex plaincc { <-[;]>+ }
    ok "Foobar" ~~ / <plaincc> | 'Foo' /, 'plain class behind a subrule: matches';
    is ~$/, 'Foobar', 'a subrule wrapping a plain class still participates (control)';
}
