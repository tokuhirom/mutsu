use v6;
use Test;

plan 5;

# A role declaration can carry BOTH dist adverbs and a parametric signature —
# adverbs first, the order Raku itself uses:
#   role JSON::Class:ver<0.0.21>:auth<zef:jonathanstowe>[Bool :$opt-in = False]
# The parser used to try the signature only BEFORE the adverbs, so this shape
# failed to parse (blocking `use JSON::Class` and everything above it:
# License::SPDX, META6, Test::META).

role R:ver<1.0>:auth<zef:test>[Bool :$b = False] {
    method flagged { $b }
}

ok R.new.flagged === False, 'bare composition uses the parameter default';
ok R.^name eq 'R', 'the role name excludes the adverbs';

class WithDefault does R {
}
ok WithDefault.new.flagged === False, 'class does R (no args) gets the default';

role S:api<2>[::T] {
    method of-type { T.^name }
}
is S[Int].new.of-type, 'Int', 'adverb followed by a type-parameter signature';

# Adverbs alone still parse (regression guard for the pre-existing shape).
role Plain:ver<0.1> {
    method p { 'p' }
}
is Plain.new.p, 'p', 'adverbs without a signature still parse';
