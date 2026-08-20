use v6;
use Test;

plan 11;

# A user-declared `multi sub postcircumfix:<[ ]>` / `postcircumfix:<{ }>` must
# intercept the bracket-subscript OPERATOR for a matching (invocant, index)
# type pair on an Instance target, ahead of the built-in AT-POS/AT-KEY
# protocol dispatch — the mechanism modules like `Array::Rounded` rely on to
# reinterpret a non-Int subscript before it ever reaches AT-POS.
# See todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md.

class PCFoo is Array {
    method AT-POS(\i) { "AT-POS(" ~ i ~ ")" }
}

multi sub postcircumfix:<[ ]>(PCFoo:D \self, Str:D $key) {
    "custom(" ~ $key ~ ")"
}

my @f is PCFoo = 1, 2, 3;
is @f[0], 'AT-POS(0)', 'plain Int index still reaches AT-POS when no candidate matches it';
is @f["hi"], 'custom(hi)', 'Str index is intercepted by the user postcircumfix:<[ ]> candidate';

# Candidate specificity: a narrower `Int:D` candidate must win over a broader
# `Any:D` one for an actual Int index, and the fractional/other-typed index
# must reach the `Any:D` candidate WITHOUT being truncated first (unlike a
# plain AT-POS dispatch, which does truncate).
class PCRounded is Array {
    method AT-POS(\i) { "AT-POS(" ~ i ~ ")" }
}

multi sub postcircumfix:<[ ]>(PCRounded:D \self, Int:D $index) {
    "int-candidate(" ~ $index ~ ")"
}
multi sub postcircumfix:<[ ]>(PCRounded:D \self, Any:D \index) {
    "any-candidate(" ~ index ~ ")"
}

my @r is PCRounded = 1, 2, 3;
is @r[2], 'int-candidate(2)', 'Int:D candidate wins over Any:D for a plain Int index';
is @r[1.5], 'any-candidate(1.5)', 'a Rat index reaches Any:D with its raw (untruncated) value';
is @r["k"], 'any-candidate(k)', 'a Str index also reaches the Any:D candidate';

# A class with no postcircumfix:<[ ]> declared at all is unaffected — the
# native AT-POS dispatch (and its existing Rat-truncation behavior) still
# applies exactly as before this change.
class PCPlain is Array {
    method AT-POS(\i) { "AT-POS(" ~ i ~ ")" }
}
my @p is PCPlain = 1, 2, 3;
is @p[1.9], 'AT-POS(1)', 'no user postcircumfix candidate: subscript still truncates before AT-POS';

# A `postcircumfix:<{ }>` candidate is checked independently of `[ ]`, keyed
# on the `is_positional` flag of the subscript syntax used.
class PCHash is Hash {
    method AT-KEY(\k) { "AT-KEY(" ~ k ~ ")" }
}
multi sub postcircumfix:<{ }>(PCHash:D \self, Int:D $key) {
    "hashcustom(" ~ $key ~ ")"
}
my %h is PCHash;
is %h{5}, 'hashcustom(5)', 'postcircumfix:<{ }> is checked independently for associative subscripts';

# A 3-argument `multi sub postcircumfix:<[ ]>(target, index, value)` candidate
# intercepts subscript ASSIGNMENT (`@obj[i] = v`) — a genuinely distinct
# multi-dispatch form from the 2-arg read candidate above, confirmed against
# real raku: assigning through a 2-arg-only candidate is itself a raku
# compile-time error ("Calling postcircumfix:<[ ]>(..., Int, Int) will never
# work"). See "Status update 2" in
# todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md.
class PCWrite {
    has @.store is rw = (1, 2, 3);
    method AT-POS(\i) { "AT-POS(" ~ i ~ ")" }
}

my @log;
multi sub postcircumfix:<[ ]>(PCWrite:D \SELF, Int:D $index, Mu $value) is rw {
    @log.push("assign(" ~ $index ~ ", " ~ $value ~ ")");
    SELF.store[$index] = $value;
    "assign-result";
}

my $w = PCWrite.new;
is ($w[0] = 42), 'assign-result',
    'assignment through a 3-arg postcircumfix candidate returns its result';
is @log[0], 'assign(0, 42)',
    '... and the candidate was invoked with (index, value)';
is $w.store[0], 42,
    "... and the candidate's own delegation actually wrote the value";

# A class with only a 2-arg (read) candidate and no matching 3-arg one falls
# back to native array assignment for `=` — mirroring the "no candidate
# declared at all" fallback for reads.
class PCReadOnly is Array {}
multi sub postcircumfix:<[ ]>(PCReadOnly:D \SELF, Str:D $key) {
    "readonly-custom(" ~ $key ~ ")"
}
my @rd is PCReadOnly = 1, 2, 3;
@rd[0] = 99;
is @rd[0], 99, 'no matching 3-arg candidate: native array assignment still applies';
