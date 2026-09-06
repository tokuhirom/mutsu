use Test;

# ADR-0073: a regex atom's candidates are produced on demand, driven by the
# continuation, so an embedded `{ ... }` block runs once per candidate the
# cursor ENTERS rather than once per candidate the engine COMPUTES.
#
# Every expectation below was measured against real `raku` (2026-09-07). The
# rows that already agreed before the change are just as important as the rows
# that did not: they are what a laziness change is most likely to break.

plan 64;

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
todo 'residue: the :g scan enumerates every end at every start position';
is $c, 4, 'A14 m:g runs the block once per position it commits to';

$c = 0; my $a15 = "aaa".subst(/ ( \w* { $c++ } ) /, 'X');
todo 'residue: the find scan re-runs the pattern at a position';
is $c, 1, 'A15 subst runs the block once';

$c = 0; "aaa" ~~ / ( \w* { $c++ } & \w* ) /;
todo 'ADR-0073: the conjunction arm is still collect-then-pick';
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
todo 'ADR-0073 Slice 2: the <subrule> boundary is still collect-then-pick';
is $e, 2, 'E1  regex subrule under a regex caller';

my $e2 = 0;
grammar E2G { regex TOP { <a> 'c' }; regex a { <b> }; regex b { \w* { $e2++ } } }
$e2 = 0; E2G.parse('aaac');
todo 'ADR-0073 Slice 2: the <subrule> boundary is still collect-then-pick';
is $e2, 2, 'E2  nested regex subrules';

my $e3 = 0;
grammar E3G { token TOP { <part> 'c' }; regex part { \w* { $e3++ } } }
$e3 = 0; E3G.parse('aaac');
todo 'ADR-0073 Slice 2: a ratcheted caller cannot backtrack into the subrule';
is $e3, 1, 'E3  regex subrule under a ratcheted caller';

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
todo 'ADR-0073 Slice 2: the <subrule> boundary is still collect-then-pick';
is @e6.join(','), 'one', 'E6  ordered alternation inside a non-ratcheted subrule';

my $e7 = 0;
grammar E7G { token TOP { :my $*N = 0; <part> 'c' }; token part { \w* { $e7++ } } }
$e7 = 0; E7G.parse('aaac');
is $e7, 1, 'E7  a $* -declaring caller does not change the count';

my $e8 = 0;
grammar E8G { token TOP { <part> 'c' }; token part { (\w*) { $e8++; make ~$0 } } }
$e8 = 0; E8G.parse('aaac');
is $e8, 1, 'E8  a make-bearing block runs once';

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
todo 'pre-existing: a non-capturing group with a block under ** N..M % loses the chain';
is $c, 3, 'F6  [ \w+ {B} ] ** 1..3 % , runs the block once per iteration';
todo 'pre-existing: a non-capturing group with a block under ** N..M % loses the chain';
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
