use Test;

plan 7;

# A `die` inside an embedded regex `{ ... }` block or `<?{ ... }>` assertion
# propagates out of the match (Rakudo semantics) instead of being swallowed
# as a mismatch. And a side-effect-only alternative (`|| { die ... }`) runs
# ONLY when no earlier alternative matched — Cro::HTTP::Header's grammar uses
# `|| <valid header> || { die "Malformed header" }` as a parse-failure arm.

grammar GCBD {
    token TOP { || "a" || { die "boom" } }
}

# 1. When the first alternative matches, the die arm must NOT run.
my $m;
lives-ok { $m = GCBD.parse("a") }, 'matching first alternative skips die arm';
ok $m.defined, 'parse succeeded';

# 2. When nothing matches, the die arm runs and the exception propagates.
throws-like { GCBD.parse("b") }, Exception, message => /boom/,
    'die arm fires when no alternative matches';

# 3. die inside a plain block propagates from a smartmatch too.
throws-like { "b" ~~ /a|{ die "boom3" }/ }, Exception, message => /boom3/,
    'die in LTM alternation code arm propagates';

# 4. die inside an assertion propagates.
throws-like { "a" ~~ /a <?{ die "boom2" }>/ }, Exception, message => /boom2/,
    'die in code assertion propagates';

# 5. A side-effect block on the winning path still runs inline.
my $ran = 0;
ok "ab" ~~ /a { $ran++ } b/, 'match with inline block';
is $ran, 1, 'inline block ran once';
