use Test;

# ADR-0073: a regex atom's candidates are produced on demand, driven by the
# continuation, so an embedded `{ ... }` block runs once per candidate the
# cursor ENTERS rather than once per candidate the engine COMPUTES.
#
# Every expectation below was measured against real `raku` (2026-09-07). The
# rows that already agreed before the change are just as important as the rows
# that did not: they are what a laziness change is most likely to break.

plan 94;

my $c;

# --- Family A: a group atom asked for all its ends -------------------------

$c = 0; "aaac" ~~ / ( \w* { $c++ } ) c /;
is $c, 2, 'A1  ( \w* {B} ) c';

$c = 0; "aaac" ~~ / ( \w+ { $c++ } ) c /;
is $c, 2, 'A2  ( \w+ {B} ) c';

$c = 0; "ab" ~~ / ( 'a'? { $c++ } ) b /;
is $c, 1, 'A3  ( a? {B} ) b';

$c = 0; "aaac" ~~ / ( [\w ** 1..4] { $c++ } ) c /;
is $c, 2, 'A4  ( [\w ** 1..4] {B} ) c';

$c = 0; "aaac" ~~ / ( \w*? { $c++ } ) c /;
is $c, 4, 'A5  frugal ( \w*? {B} ) c';

$c = 0; "aaa" ~~ / ( \w* { $c++ } ) /;
is $c, 1, 'A6  ( \w* {B} ) with no continuation at all';

$c = 0; "aaa" ~~ / ( \w+ { $c++ } ) $ /;
is $c, 1, 'A7  ( \w+ {B} ) $';

$c = 0; "aaa" ~~ / [ \w* { $c++ } ] /;
is $c, 1, 'A8  non-capturing [ \w* {B} ]';

$c = 0; "aaac" ~~ / ( ( \w* { $c++ } ) ) c /;
is $c, 2, 'A9  nested groups';

$c = 0; "aaac" ~~ / ( [ \w* ] { $c++ } ) c /;
is $c, 2, 'A10 block after an inner group';

$c = 0; "aaac" ~~ / ( \w* { $c++ } ) <?before c> /;
is $c, 2, 'A11 group before a positive lookahead';

$c = 0; "aaac" ~~ / ( \w* { $c++ } ) <!before x> /;
is $c, 1, 'A12 group before a negative lookahead';

$c = 0; "aaac" ~~ / :my $*Z = 0; ( \w* { $c++; $*Z = 1 } ) c /;
is $c, 2, 'A13 a $* -mentioning block counts like any other';

$c = 0; my @a14 = "aa bb" ~~ m:g/ ( \w* { $c++ } ) /;
is $c, 4, 'A14 m:g runs the block once per position it commits to';

$c = 0; my $a15 = "aaa".subst(/ ( \w* { $c++ } ) /, 'X');
is $c, 1, 'A15 subst runs the block once';

$c = 0; "aaa" ~~ / ( \w* { $c++ } & \w* ) /;
is $c, 1, 'A16 conjunction runs the block once';

# A17: a `die` in a block on a candidate raku never enters must not abort the
# match. This is the severity of the whole family, not just a wrong counter.
my $d = 0;
my $died = False;
try {
    "aaac" ~~ / ( \w* { $d++; $d > 3 ?? die('boom') !! 1 } ) c /;
    CATCH { default { $died = True } }
}
nok $died, 'A17 a die in a never-entered candidate does not abort the match';

# --- Family B: already demand-driven; these must not move ------------------

$c = 0; "aaa" ~~ / \w* { $c++ } /;
is $c, 1, 'B1  top-level \w* {B}';

$c = 0; "aaac" ~~ / \w* { $c++ } c /;
is $c, 2, 'B2  top-level \w* {B} c';

$c = 0; "aaa" ~~ / ( { $c++ } \w* ) /;
is $c, 1, 'B3  block before the quantifier';

$c = 0; "aaa" ~~ / ( \w* ) { $c++ } /;
is $c, 1, 'B4  block after the group';

$c = 0; "ab" ~~ / ( 'a' { $c++ } ) b /;
is $c, 1, 'B5  fixed-width group';

$c = 0; "ab" ~~ / ( 'a' { $c++ } ) ( 'b' { $c++ } ) /;
is $c, 2, 'B6  two fixed-width groups';

$c = 0; "aaac" ~~ / :r ( \w* { $c++ } ) c /;
is $c, 5, 'B7  ratcheted group retries at each start position';

$c = 0; "aaa" ~~ / :r ( \w* { $c++ } ) /;
is $c, 1, 'B8  ratcheted group, first position wins';

$c = 0; "aab" ~~ / [ \w+ { $c++ } ]+ b /;
is $c, 3, 'B9  + of a group with an inner +';

$c = 0; "aab" ~~ / ( \w+ { $c++ } )* b /;
is $c, 3, 'B10 * of a capturing group with an inner +';

$c = 0; "ab" ~~ / [ \w { $c++ } ]? b /;
is $c, 1, 'B11 optional group';

$c = 0; "aaab" ~~ / [ \w { $c++ } ] ** 2..3 b /;
is $c, 3, 'B12 ** 2..3 of a group';

$c = 0; "aab" ~~ / ( [ \w { $c++ } ]* ) b /;
is $c, 3, 'B13 block inside the repeated unit';

$c = 0; "aaac" ~~ / ( [ \w { $c++ } ]* ) c /;
is $c, 4, 'B14 block inside the repeated unit, with backtracking';

# --- Family C: unordered `|` alternation -----------------------------------
# C1 is the control: raku genuinely enters BOTH branches, because the LTM
# winner `'bc'` is rejected by the `'cd'` that follows.

$c = 0; "abcd" ~~ / 'a' [ 'b' { $c += 1 } | 'bc' { $c += 10 } ] 'cd' /;
is $c, 11, 'C1  both branches entered (LTM winner rejected by the continuation)';

$c = 0; "abcd" ~~ / 'a' [ 'bc' { $c += 10 } | 'b' { $c += 1 } ] /;
is $c, 10, 'C2  only the LTM winner is entered';

$c = 0; "abcd" ~~ / 'a' [ 'b' { $c += 1 } | 'bc' { $c += 10 } ] /;
is $c, 10, 'C3  written order does not change which branch is entered';

$c = 0; "abcd" ~~ / :r 'a' [ 'bc' { $c += 10 } | 'b' { $c += 1 } ] 'cd' /;
is $c, 10, 'C4  ratchet commits to the LTM winner';

$c = 0; "abcd" ~~ / 'a' [ 'bcd' { $c += 100 } | 'bc' { $c += 10 } | 'b' { $c += 1 } ] /;
is $c, 100, 'C5  three branches, only the winner is entered';

# --- The `<!>` fate: a branch's prefix ends BEFORE an always-fail assertion ---
# Demand-driven `|` only enters the branches the ranking puts first, so a branch
# whose declarative prefix is mis-measured is never entered at all. `<!>` is a
# Cursor method in Rakudo, so its NFA has no edge for it: the prefix ends before
# it rather than the whole branch measuring as unmatchable. All four rows were
# measured against raku.

my $lc = 0;
my $ldied = False;
try {
    'food' ~~ / 'foo' | ( 'food' <!> || { $lc++; die 'boom' } ) /;
    CATCH { default { $ldied = True } }
}
ok $ldied, 'L1  the <!> branch outranks a shorter literal and is entered';

$c = 0;
is ~(('food' ~~ / 'foo' | ( 'food' <!> || { $c++; 1 } ) /) // 'NONE'), '',
    'L2  and its zero-width || fallback wins the match';
is $c, 1, 'L3  running its block exactly once';

is ~('food' ~~ / 'foo' | ( 'food' <!> || 'doof' ) /), 'foo',
    'L4  when the whole branch fails, LTM falls back to the shorter literal';

# --- Family D: ordered `||` alternation (already continuation-driven) ------

$c = 0; "abcd" ~~ / 'a' [ 'b' { $c += 1 } || 'bc' { $c += 10 } ] 'cd' /;
is $c, 1, 'D1  first branch succeeds, second never entered';

$c = 0; "abcd" ~~ / 'a' [ 'bc' { $c += 10 } || 'b' { $c += 1 } ] 'cd' /;
is $c, 11, 'D2  first branch entered then abandoned, second entered';

$c = 0; "abcd" ~~ / 'a' [ 'b' { $c += 1 } || 'bc' { $c += 10 } ] /;
is $c, 1, 'D3  no continuation, first branch wins';

# --- Family E: `<subrule>` calls -------------------------------------------

my $e = 0;
grammar E1G { regex TOP { <part> 'c' }; regex part { \w* { $e++ } } }
$e = 0; E1G.parse('aaac');
is $e, 2, 'E1  regex subrule under a regex caller';

my $e2 = 0;
grammar E2G { regex TOP { <a> 'c' }; regex a { <b> }; regex b { \w* { $e2++ } } }
$e2 = 0; E2G.parse('aaac');
is $e2, 2, 'E2  nested regex subrules';

my $e3 = 0;
grammar E3G { token TOP { <part> 'c' }; regex part { \w* { $e3++ } } }
$e3 = 0; my $e3m = E3G.parse('aaac');
is $e3, 1, 'E3  regex subrule under a ratcheted caller';
# A ratcheted caller commits to the subrule's highest-priority end, so `\w*`
# eats the trailing `c` and TOP's literal has nothing left: raku does NOT match
# here, and the count above is exactly why -- there is no second candidate to
# fall back to.
nok $e3m.defined, 'E3a and the ratcheted caller cannot backtrack, so the parse fails';

my $e3b = 0;
grammar E3BG { token TOP { <part> 'a' }; regex part { \w* { $e3b++ } } }
my $e3bm = E3BG.parse('aaa');
is $e3b, 1, 'E3b  the subrule body runs once even when the caller then fails';
nok $e3bm.defined, 'E3c and that failure is raku behaviour, not a lost candidate';

# The Slice-2 guard: a rule that can call a rule stays on the full walk, so the
# left-recursion growing-seed loop still sees its own re-entry. `<term>` ranks
# ahead of the recursive branch, so a first-only walk of `expr` would stop on
# `1` and lose the parse.
#
# These two rows have no `raku` oracle -- Rakudo has no growing-seed loop and
# hangs on a left-recursive rule. They pin a mutsu capability against the
# regression an unguarded Slice 2 would cause; every other row in this file was
# measured against raku.
grammar E3LR {
    token TOP  { <expr> }
    token expr { <term> | <expr> '+' <term> }
    token term { \d+ }
}
is ~(E3LR.parse('1+2+3') // ''), '1+2+3',
    'E3d  left recursion under a ratcheted caller still grows its seed';
grammar E3LR2 {
    token TOP  { <expr> }
    token expr { <expr> '+' <term> | <term> }
    token term { \d+ }
}
is ~(E3LR2.parse('1+2+3') // ''), '1+2+3',
    'E3e  and with the recursive branch declared first';

# A proto/multi subrule under a ratcheted caller keeps its rank-then-match
# dispatch (ADR-0046) and runs the winner's block once.
my $e3f = 0;
grammar E3FG {
    token TOP { <part> 'c' }
    proto token part {*}
    multi token part:sym<a> { \w* { $e3f++ } }
}
E3FG.parse('aaac');
is $e3f, 1, 'E3f  a proto subrule under a ratcheted caller runs its winner once';

# Control: a QUANTIFIED subrule under a ratcheted caller grows its chain one
# iteration at a time already (`walk_quant_chain`), and must keep doing so.
my $e3g = 0;
grammar E3GG { token TOP { <part>+ 'c' }; regex part { \w { $e3g++ } } }
E3GG.parse('aaac');
is $e3g, 4, 'E3g  a quantified subrule still runs once per iteration entered';

my $e4 = 0;
grammar E4G { regex TOP { <part> 'c' }; token part { \w* { $e4++ } } }
$e4 = 0; E4G.parse('aaac');
is $e4, 1, 'E4  ratcheted subrule under a regex caller';

my $e5 = 0;
grammar E5G { token TOP { <part> 'c' }; token part { \w* { $e5++ } } }
$e5 = 0; E5G.parse('aaac');
is $e5, 1, 'E5  token/token';

my @e6;
grammar E6G {
    regex TOP  { <part> 'cd' }
    regex part { 'a' [ 'b' { @e6.push('one') } || 'bc' { @e6.push('two') } ] }
}
@e6 = (); E6G.parse('abcd');
is @e6.join(','), 'one', 'E6  ordered alternation inside a non-ratcheted subrule';

my $e7 = 0;
grammar E7G { token TOP { :my $*N = 0; <part> 'c' }; token part { \w* { $e7++ } } }
$e7 = 0; E7G.parse('aaac');
is $e7, 1, 'E7  a $* -declaring caller does not change the count';

my $e8 = 0;
grammar E8G { token TOP { <part> 'c' }; token part { (\w*) { $e8++; make ~$0 } } }
$e8 = 0; E8G.parse('aaac');
is $e8, 1, 'E8  a make-bearing block runs once';

# E9: a NON-LEAF rule under a ratcheted caller. The first half of Slice 2 kept
# such a rule on the full walk (its syntactic guard admitted leaf bodies only);
# the call-graph analysis proves `part` cannot reach a call to `part`, so it is
# streamed like any other.
my $e9 = 0;
grammar E9G {
    token TOP   { <part> 'c' }
    regex part  { <inner> }
    regex inner { \w* { $e9++ } }
}
E9G.parse('aaac');
is $e9, 1, 'E9  a non-leaf subrule under a ratcheted caller runs its block once';

# E10: mutual recursion. `a` can reach `a` (through `b`), so the call graph
# refuses it and the growing-seed loop keeps the full end set. No raku oracle --
# Rakudo hangs on this grammar -- so this pins mutsu's own capability, exactly
# as E3d/E3e do.
grammar E10G {
    regex TOP { <a> 'c' }
    regex a   { <b> | 'zz' }
    regex b   { <a> | \w* }
}
is ~(E10G.parse('aaac') // ''), 'aaac', 'E10 mutual recursion still parses';

# E11/E12/E13 were measured against raku and agreed before this change; they are
# the shapes a streamed `<subrule>` is most likely to break.
my $e11 = 0;
grammar E11G { rule TOP { <part> 'c' }; token part { \w+ { $e11++ } } }
is ~(E11G.parse('aaa c') // ''), 'aaa c', 'E11 a `rule` caller (with <.ws>) still parses';
is $e11, 1, 'E11a and runs the subrule block once';

my $e12 = 0;
grammar E12G { regex TOP { <part>+ 'c' }; regex part { \w { $e12++ } } }
E12G.parse('aaac');
is $e12, 4, 'E12 a quantified subrule under a NON-ratcheted caller';

my $e13 = 0;
grammar E13G { regex TOP { <a> 'c' }; regex a { <b> 'a' }; regex b { \w* { $e13++ } } }
is ~(E13G.parse('aaac') // ''), 'aaac', 'E13 backtracking through two subrule levels';
is $e13, 3, 'E13a and the innermost block runs once per end entered';

# --- Family F: separated quantifiers ---------------------------------------

$c = 0; "line\nline2\nline3" ~~ rx| ( \V* { $c++ } ) *%% \n |;
is $c, 3, 'F1  ( \V* {B} ) *%% \n  (the headline repro)';

$c = 0; "line\nline2\nline3" ~~ rx| ( \V* { $c++ } ) *% \n |;
is $c, 3, 'F2  ( \V* {B} ) *% \n';

$c = 0; "a,b,c" ~~ / ( \w+ { $c++ } ) +% ',' /;
is $c, 3, 'F3  ( \w+ {B} ) +% ,';

$c = 0; "a,b,c" ~~ / :r ( \w+ { $c++ } ) +% ',' /;
is $c, 3, 'F4  ratcheted ( \w+ {B} ) +% ,';

$c = 0; "a,b,c" ~~ / ( \w { $c++ } ) +%% ',' /;
is $c, 3, 'F5  ( \w {B} ) +%% ,';

$c = 0; my $f6 = "a,b,c" ~~ / [ \w+ { $c++ } ] ** 1..3 % ',' /;
is $c, 3, 'F6  [ \w+ {B} ] ** 1..3 % , runs the block once per iteration';
is ~($f6 // ''), 'a,b,c', 'F6b and matches the whole chain';

$c = 0; "line\nline2" ~~ rx| :r ( \V* { $c++ } ) *%% \n |;
is $c, 2, 'F7  ratcheted separated quantifier';

$c = 0; "a,b,c" ~~ / [ \w+ ] +% ',' { $c++ } /;
is $c, 1, 'F8  block after the separated quantifier';

# --- the separated quantifier still matches what it used to ----------------

is ~("a,b,c" ~~ / [ \w+ ] ** 1..3 % ',' /), 'a,b,c', 'G1  ** 1..3 % , chain';
is ~("a,b,c" ~~ / [ \w+ ] ** 2 % ',' /), 'a,b', 'G2  ** 2 % , chain';
is ~("a,b,c" ~~ / [ \w+ ] +% ',' /), 'a,b,c', 'G3  +% , chain';
is (("a,b,c" ~~ / ( \w+ ) ** 1..3 % ',' /)[0].map(~*).join('|')), 'a|b|c',
    'G4  ** 1..3 % , folds each iteration into $0';

# --- A16b: the conjunction's other shapes, all verified against raku ---------
# The first branch is now walked lazily and the OTHER branches keep the eager
# yes/no probe (`regex_match_branch_ending_at` asks about one end, so there is
# no candidate set to stream). These pin that the merge, the priority order and
# the backtracking into a shorter first-branch match are unchanged.

is ("aaa" ~~ / ( \w* & \w* ) /).Str, 'aaa', 'A16b conjunction of two greedy branches';
is ("abc" ~~ / (\w+ & <[a..c]>+) /).Str, 'abc', 'A16c a character-class branch';
nok ("abc" ~~ / (\d+ & \w+) /), 'A16d a branch that cannot match fails the whole conjunction';
is ("abc" ~~ / (\w+) & (\w+) /).Str, 'abc', 'A16e captures from both sides are kept';
is ("abcd" ~~ / ( \w+ & \w\w ) /).Str, 'ab', 'A16f the shared end is the shorter branch';
is ("abc" ~~ /^ [ \w+ & 'abc' ] $/).Str, 'abc', 'A16g anchored conjunction';

{
    # The continuation rejects the first (longest) end, so the walk comes back
    # for the second -- and the block runs exactly twice, as raku does.
    my $n = 0;
    is ("aaab" ~~ / ( \w* { $n++ } & \w* ) b /).Str, 'aaab',
        'A16h backtracking into the conjunction still matches';
    is $n, 2, 'A16i ... and runs the block once per end actually entered';
}

# --- `** N..M % sep` on a NON-capturing atom is string-expanded (the native
# separated-quantifier path is reserved for capture-bearing atoms), and that
# expansion has to be greedy AND capture-free.

is ~("a,b,c" ~~ / [ \w+ ] ** 2..3 % ',' /), 'a,b,c', 'H1  ** 2..3 % , takes the longest chain';
is ("a,b,c" ~~ / [ \w+ ] ** 1..3 % ',' /).list.elems, 0,
    'H2  and introduces no positional capture';
my $h3 = 0;
is ~("a,b,c" ~~ / [ \w+ { $h3++ } ] ** 2..3 % ',' /), 'a,b,c', 'H3  the same with a block';
is $h3, 3, 'H4  whose block ran once per iteration';
is ~("a,b" ~~ / [ \w+ ] ** 1..3 % ',' /), 'a,b', 'H5  a chain shorter than the max still matches';
is ~("a" ~~ / [ \w+ ] ** 1..3 % ',' /), 'a', 'H6  and a single element does too';
nok ("a" ~~ / ^ [ \w+ ] ** 2..3 % ',' $ /), 'H7  but the minimum is still enforced';
is ~("a,b,c," ~~ / [ \w+ ] ** 1..3 %% ',' /), 'a,b,c,', 'H8  %% takes the trailing separator';
