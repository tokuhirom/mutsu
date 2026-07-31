use Test;

plan 3;

# `Grammar.parse($s, :rule<proto-name>, :actions(...))` starting DIRECTLY at a
# proto token must dispatch the winning candidate's `:sym<...>` action method.
# Cro::Uri::HTTP.parse-request-target does exactly this with
# `:rule('request-target')` where request-target is a proto token.

grammar GPRS {
    proto token t { * }
    token t:sym<a> { "a" }
    token t:sym<b> { "b" }
}
class GPRS-Actions {
    method t:sym<a>($/) { make "GOT-A" }
    method t:sym<b>($/) { make "GOT-B" }
}

is GPRS.parse("a", :actions(GPRS-Actions), :rule("t")).ast, "GOT-A",
    'sym<a> action dispatched for :rule proto parse';
is GPRS.parse("b", :actions(GPRS-Actions), :rule("t")).ast, "GOT-B",
    'sym<b> action dispatched for :rule proto parse';

# A nested proto subrule still dispatches per-variant (regression guard).
grammar GPRS2 {
    token TOP { <t> }
    proto token t { * }
    token t:sym<a> { "a" }
}
class GPRS2-Actions {
    method TOP($/) { make $<t>.made }
    method t:sym<a>($/) { make "NESTED-A" }
}
is GPRS2.parse("a", :actions(GPRS2-Actions)).ast, "NESTED-A",
    'nested proto sym action still dispatched';
