use Test;

plan 1;

# Ranking multi METHOD candidates by type distance must walk each
# candidate's POSITIONAL parameters against only the call's positional
# arguments — never against the raw argument list, which interleaves named
# arguments (encoded as `Pair` values) among the positionals in call-site
# order. `method_candidate_type_distance` indexed the raw list directly, so
# a candidate with an unfilled trailing optional positional parameter (e.g.
# `Int $index = 10`, no argument supplied for it because the call only gave
# one positional) read the NEXT array slot for that parameter's distance
# check — which, whenever the call also passed any named argument at all,
# was that named argument's `Pair` value rather than nothing. Scoring a
# `Pair` against `Int` came back "unrelated" and added a large penalty a
# same-arity, fully untyped catch-all never paid (it has no such parameter to
# score), so the catch-all won even though it should have lost on type
# narrowness alone. This is the pattern behind ASN::BER's
# `Serializer.serialize($value, ..., :$debug, :$mode)` always reaching its
# `$unknown-type` fallback whenever any named argument was passed alongside
# the value.

class Foo {
    multi method describe(Str $s, Int $index = 10, :$debug) {
        "typed candidate, index=$index";
    }
    multi method describe($unknown, :$debug) {
        "catch-all: {$unknown.raku}";
    }
}

is Foo.new.describe("hi", :debug(False)), 'typed candidate, index=10',
    'a named argument does not misalign the positional distance check';

# vim: expandtab shiftwidth=4
