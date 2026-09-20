use Test;

plan 1;

# A candidate with an optional/default trailing positional parameter must
# still compete on TYPE narrowness against a same-arity, fully untyped
# catch-all — not lose automatically just because the catch-all happens to
# be the only candidate registered at the call's exact arity. Bare-name
# multi-sub resolution gathers the exact-arity candidate group first and, as
# a performance fast path, returns its winner immediately whenever none of
# THOSE candidates carries an optional parameter — reasoning that such a
# candidate is already narrower than any wider fallback. That reasoning only
# holds when the exact-arity winner is itself typed; an untyped catch-all
# (`multi f($x) {...}`) is the WIDEST possible signature, so the fast path
# must not fire when it is present, or it silently wins over a strictly
# narrower flexible-arity candidate declared elsewhere (`multi f(Str $s, Int
# $index = 10) {...}` called as `f("hi")` — the pattern behind ASN::BER's
# `Serializer.serialize`/`Parser.parse` always reaching their `NYI`/unknown-
# type fallback whenever a real typed candidate had a trailing optional
# parameter).

multi sub pick(Str $s, Int $index = 10) {
    "typed candidate, index=$index";
}
multi sub pick($unknown) {
    "catch-all: {$unknown.raku}";
}

is pick("hi"), 'typed candidate, index=10',
    'a typed flexible-arity candidate beats an untyped exact-arity catch-all';

# vim: expandtab shiftwidth=4
