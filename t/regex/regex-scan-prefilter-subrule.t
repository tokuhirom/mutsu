use Test;

# ADR-0099 Stage 1 (#8272): the unanchored-scan prefilter derives a
# first-character set THROUGH a `<subrule>`, keyed by invocant package and
# `TOKEN_DEFS_GEN` (constraint 3) instead of declining on every rule name.
#
# The prefilter can only ever be wrong by dropping a match it should have
# found, and every assertion here is a match it must still find. The Rust-side
# `tests/regex_prefilter_differential.rs` pins the same property against
# `MUTSU_REGEX_PREFILTER=off`; this file pins the behaviour a Raku program can
# actually observe.

plan 26;

grammar Kw {
    token kw       { 'zzzq' | 'qqqz' }
    token outer    { <inner> }
    token inner    { <[xy]> \d }
    token opt      { \d* }
    token leftrec  { <leftrec> 'a' | 'b' }
    token rightrec { 'a' <rightrec> | 'b' }
}

# --- the set is the union over the rule's branches ------------------------
ok 'aa zzzq bb' ~~ / <Kw::kw> /,       'first branch of a rule is found';
ok 'aa qqqz bb' ~~ / <Kw::kw> /,       'second branch of a rule is found';
nok 'aa zzq bb' ~~ / <Kw::kw> /,       'a near miss is still a miss';
is ('aa qqqz bb' ~~ / <Kw::kw> /).Str, 'qqqz', 'the match text is unchanged';

# --- the walk follows the call chain -------------------------------------
is ('ab x7 cd' ~~ / <Kw::outer> /).Str, 'x7', 'a rule reached through a rule';
nok 'ab z7 cd' ~~ / <Kw::outer> /,      'and its first-set still excludes';

# --- a rule that can match empty rules nothing out ------------------------
ok 'abc' ~~ / <Kw::opt> 'b' /, 'a nullable rule leaves the set open';
is ('abc' ~~ / <Kw::opt> 'b' /).Str, 'b', 'and the match is the right one';

# --- recursion ------------------------------------------------------------
is ('xbz' ~~ / <Kw::leftrec> /).Str,  'b', 'a left-recursive rule still matches';
is ('zzab' ~~ / <Kw::rightrec> /).Str, 'ab', 'a right-recursive rule still matches';
nok 'zzq' ~~ / <Kw::rightrec> /,       'and fails where it should';

# --- the same pattern source, two packages -------------------------------
# `/ <x> /` is one cache entry shared by both subs, so a first-set frozen
# against the first package to scan would drop every match in the second.
package A {
    our token x { 'aaa' }
    our sub scan($s) { so $s ~~ / <x> / }
}
package B {
    our token x { 'bbb' }
    our sub scan($s) { so $s ~~ / <x> / }
}
ok  A::scan('zz aaa'), 'package A resolves its own rule';
ok  B::scan('zz bbb'), 'package B resolves its own rule';
nok A::scan('zz bbb'), 'package A does not see B\'s rule';
nok B::scan('zz aaa'), 'package B does not see A\'s rule';
ok  A::scan('zz aaa'), 'package A again, after B was scanned';
ok  B::scan('zz bbb'), 'package B again, after A was scanned';

# --- `:i` does not reach into a rule body --------------------------------
# Both mutsu and rakudo scope `:i` to the pattern that declares it, so the
# analysis must read each sub-pattern's own flag rather than OR-ing the
# enclosing one down.
grammar Case {
    token up   { 'ZQ' }
    token anyc { :i 'ZQ' }
}
nok 'aa zq bb' ~~ / :i <Case::up> /, ':i does not fold a called rule\'s body';
ok  'aa ZQ bb' ~~ / :i <Case::up> /, 'the rule body matches as written';
ok  'aa zq bb' ~~ / <Case::anyc> /,  'a rule\'s own :i still applies';

# --- shapes the analysis must decline on, which must still match ---------
grammar Blocky {
    token thing { { 1 } 'q' }
}
ok 'abq' ~~ / <Blocky::thing> /, 'a rule opening with a code block matches';

grammar Args {
    token digits($n) { \d ** {$n} }
    token top        { <digits(2)> }
}
is ('ab123' ~~ / <Args::top> /).Str, '12', 'a parameterized rule call matches';

my regex lexthing { 'zq' };
is ('aa zq bb' ~~ / <&lexthing> /).Str, 'zq', 'a lexical regex reference matches';

# --- a proto's candidates are all admitted -------------------------------
grammar Ops {
    proto token op { * }
    token op:sym<plus> { '+' }
    token op:sym<star> { '*' }
}
is ('a + b' ~~ / <Ops::op> /).Str, '+', 'a proto candidate is found';
is ('a * b' ~~ / <Ops::op> /).Str, '*', 'every proto candidate is admitted';

# --- a global scan yields every occurrence -------------------------------
grammar Kw2 { token kw { 'aa' | 'bb' } }
is ('xaaybbz' ~~ m:g/ <Kw2::kw> /).join(','), 'aa,bb', 'a :g scan finds them all';
