use v6;
use Test;

# RakuAST Phase 2 slice 12 (ADR-0011): explicit-signature `for @a -> $x { }`.
# Unlike an implicit-topic `for` (whose body is a topic-marked Block, slice 6),
# an explicit signature makes the body a PointyBlock carrying that signature.
# Expected gists captured verbatim from Rakudo; this file passes under BOTH
# mutsu and raku.

plan 3;

# --- single explicit parameter ----------------------------------------------
# The `for` body is a PointyBlock carrying the signature (NOT an implicit-topic
# Block), with the loop variable as its single parameter target.
my $sig-for = Q[my @a; for @a -> $x { $x }].AST.gist;
ok $sig-for.contains('body   => RakuAST::PointyBlock.new(')
    && $sig-for.contains('name => "\$x"')
    && !$sig-for.contains('implicit-topic'),
    'explicit-signature for -> PointyBlock body with the loop variable';

# --- two explicit parameters render two Parameters --------------------------
is Q[my @a; for @a -> $a, $b { $a }].AST.gist.comb(/'RakuAST::Parameter.new'/).elems, 2,
    'for -> $a, $b -> two Parameters in the signature';

# --- implicit-topic for (no signature) is unchanged -------------------------
is Q[for 1..3 { $_ }].AST.gist.contains('implicit-topic => True'), True,
    'implicit-topic for still uses a topic Block, not a PointyBlock';
