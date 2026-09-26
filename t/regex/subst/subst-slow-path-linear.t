use Test;

# #9143: `s:g///`, the regex `.subst` slow path (closure / :nth / :x / :c / :p)
# and the literal `.subst` slow path used to be super-linear. These pin both
# the results the rewritten scans produce and that long subjects finish.

plan 22;

# --- regex slow path: one leftmost scan, not every end at every start -------
is "aaaa".subst(/a+/, "b", :c(2)), "aab", ':c(N) starts the scan at N';
is "aXbXcX".subst(/.*?X/, { "<$/>" }, :g), "<aX><bX><cX>",
    'a frugal quantifier keeps its frugal length under :g';
is "aaXaa".subst(/a/, "b", :p(1), :g), "abXbb", ':p(N) anchors only the first match';
is "aaXaa".subst(/a/, "b", :p(2), :g), "aaXaa", ':p(N) with no match at N substitutes nothing';
is "abcabcabc".subst(/b/, "X", :nth(1, 3)), "aXcabcaXc", ':nth list';
is "abcabcabc".subst(/b/, "X", :nth(2..*)), "abcaXcaXc", ':nth(2..*) sees every match';
is "abcabcabc".subst(/b/, "X", :x(4)), "abcabcabc", ':x(N) with too few matches';
is "abcabc".subst(/(b)(c)/, { "[$1$0]" }, :g), "a[cb]a[cb]", 'closure sees captures per match';
is "あいうあいう".subst(/い/, { "<" ~ $/.from ~ ">" }, :g), "あ<1>うあ<4>う",
    'closure $/ offsets are char offsets';
my $runs = 0;
"aa bb".subst(/(\w+) { $runs++ }/, { "<$0>" }, :x(1));
ok $runs < 3, ':x(1) stops scanning once it has its match';

# --- literal slow path ------------------------------------------------------
is "あいうあいう".subst("い", "I", :nth(2)), "あいうあIう", 'literal :nth on a non-ASCII subject';
is "あいうあいう".subst("い", -> $m { $m.from }, :g), "あ1うあ4う", "literal closure match offsets";

# --- s:g/// -----------------------------------------------------------------
$_ = "あ,い,う";
s:g/","/;/;
is $_, "あ;い;う", 's:g/// on a non-ASCII subject';
is $/[1].from, 3, 's:g/// $/ list keeps each match offset';
is $/[1].orig, "あ,い,う", 'every $/ Match reports the original subject';
$_ = "a1b22c333";
s:nth(1,3)/(\d+)/<$0>/;
is $_, "a<1>b22c<333>", 's:nth(1,3)/// pairs each match with its own captures';
$_ = "a1b22c333";
s:x(2)/(\d+)/{ $0 * 2 }/;
is $_, "a2b44c333", 's:x(2)/// with a code replacement';

# --- long subjects (quadratic before #9143) ---------------------------------
my $c = "a," x 20000;
$c ~~ s:g/","/;/;
is $c.chars, 40000, 's:g/// over 20000 matches';
is $/.elems, 20000, '... and $/ holds every match';
is ("a" x 5000).subst(/a+/, { "b" }), "b", 'regex closure subst on a long run';
is ("a," x 20000).subst(",", ";", :x(*)).substr(0, 4), "a;a;", 'literal :x(*) over 20000 matches';
is ("あ," x 20000).subst(",", { "" }, :g).chars, 20000, 'literal closure :g on a long non-ASCII subject';
