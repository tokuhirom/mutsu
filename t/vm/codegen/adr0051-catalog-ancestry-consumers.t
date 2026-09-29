use Test;

# ADR-0051 P2 remainder (#9948): type matching, `.isa`, signature `~~`,
# multi-dispatch narrowness and `are()` all read built-in ancestry from the
# builtin type catalog, and a `Cool`-subtype-only method (`succ`, `base`,
# `lazy`, `polymod`, ...) is no longer answered for a receiver whose own
# ancestry lacks it. Every expectation below is raku-verified (2026-09-29).

plan 43;

# The `Cool` allowlist and `isa_check`'s variant table used to disagree with
# Rakudo for these.
ok (1, 2).Seq.isa(Cool), 'a Seq isa Cool';
ok (1..3).isa(Cool), 'a Range isa Cool';
ok Nil.isa(Cool), 'Nil isa Cool';
nok (1 => 2) ~~ Cool, 'a Pair is not Cool';
nok \(1) ~~ Cool, 'a Capture is not Cool';
nok Date.today ~~ Cool, 'a Date is not Cool';
ok <1> ~~ Cool, 'an allomorph is Cool';
ok /a/.isa(Block), 'a Regex isa Block';
nok Code.isa(Block), 'Code is not a Block';

# Multi-dispatch narrowness: `Real` is narrower than `Numeric`, and `Pair`
# carries no `Cool` ancestor.
multi f(Numeric $) { 'Numeric' }
multi f(Real $)    { 'Real' }
is f(1), 'Real', 'Int prefers Real over Numeric';
is f(1/2), 'Real', 'Rat prefers Real over Numeric';
is f(True), 'Real', 'Bool prefers Real over Numeric';
is f(now), 'Real', 'Instant prefers Real over Numeric';
multi h(Cool $) { 'Cool' }
multi h(Any $)  { 'Any' }
is h(1 => 2), 'Any', 'a Pair is not narrowed to Cool';
is h(now), 'Cool', 'an Instant narrows to Cool';

# Signature smartmatch.
ok :(Seq $) ~~ :(Cool $), 'Seq signature is narrower than Cool';
nok :(Pair $) ~~ :(Cool $), 'Pair signature is not narrower than Cool';
ok :(Instant $) ~~ :(Numeric $), 'Instant signature is narrower than Numeric';
nok :(Seq $) ~~ :(Positional $), 'Seq is not Positional';
ok :(Stash $) ~~ :(Associative $), 'Stash is Associative';
nok :(Junction $) ~~ :(Any $), 'Junction is not Any';
ok :(Match $) ~~ :(Cool $), 'Match is Cool';

# `are()` asks the same oracle.
ok [(1, 2).Seq].are(Cool), 'are(Cool) accepts a Seq';
nok (try [1 => 2].are(Cool)), 'are(Cool) rejects a Pair';
nok (try [Date.today].are(Cool)), 'are(Cool) rejects a Date';

# Compile-time default checks read the catalog too.
my Real $r is default(3) = 4;
is $r, 4, 'an Int default satisfies Real';

# Receiver-blind cascades no longer answer these.
"a1" ~~ /a./;
dies-ok { $/.succ }, 'Match cannot succ';
dies-ok { $/.pred }, 'Match cannot pred';
dies-ok { $/.parse-base(16) }, 'Match cannot parse-base';
dies-ok { 5.lazy }, 'Int cannot lazy';
dies-ok { "x".lazy }, 'Str cannot lazy';
dies-ok { (1 => 2).lazy }, 'Pair cannot lazy';
dies-ok { (1+2i).polymod(2) }, 'Complex cannot polymod';
dies-ok { "10".base(2) }, 'Str cannot base';
dies-ok { "10".polymod(2) }, 'Str cannot polymod';
dies-ok { "abc".bytes }, 'Str cannot bytes';
throws-like { 5.hyper }, X::Method::NotFound, 'Int cannot hyper';

# ...while their genuine owners keep them.
is "a".succ, 'b', 'Str.succ';
is 10.base(2), '1010', 'Int.base';
is (1, 2).Seq.hyper.list, (1, 2), 'Seq.hyper (Iterable)';
is {a => 1}.race.elems, 1, 'Hash.race (Iterable)';

# `Duration` divides its Real value.
is Duration.new(3).polymod(2), (1, 1), 'Duration.polymod';
is Duration.new(7).polymod(2, 2), (1, 1, 1), 'Duration.polymod with two divisors';
