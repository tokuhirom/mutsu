use v6;
use Test;

plan 8;

# ADR-0046 Decision 1: proto-token candidates are ranked by the ONE LTM ranking
# primitive (`ltm_branch_rank_key`) at every call site, i.e. by
# `(declarative prefix length desc, litlen desc, declaration order asc)`.
#
# This file covers ADR-0046 §2.2 for *mechanism 1* -- the `parse(:rule<...>)` /
# outermost-proto entry point (`eval_token_call_values_at`). Before Slice 3 that
# site ranked on declarative prefix length alone, with no `litlen` tie-break.
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
