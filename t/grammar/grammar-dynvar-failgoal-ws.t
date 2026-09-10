use Test;

plan 51;

# ---------------------------------------------------------------------------
# <?ww> / <!ww> -- "within word" zero-width assertions.
#
# <?ww> succeeds only when the position has a word character on BOTH sides, so
# it is NOT the negation of <?wb>: both are false between two non-word chars.
# ---------------------------------------------------------------------------

my $subject = "abc\ndef\n-==\nghi";

ok  $subject ~~ /de<?ww>/,  '<?ww> between two word chars';
ok  $subject ~~ /<?ww>ef/,  '<?ww> before a word char with a word char behind';
nok $subject ~~ /<?ww>gh/,  '<?ww> fails with a non-word char behind';
nok $subject ~~ /bc<?ww>/,  '<?ww> fails with a non-word char ahead';
nok $subject ~~ /<?ww>ab/,  '<?ww> fails at start of string';
nok $subject ~~ /hi<?ww>/,  '<?ww> fails at end of string';

nok $subject ~~ /de<!ww>/,  '<!ww> fails between two word chars';
nok $subject ~~ /<!ww>ef/,  '<!ww> fails with a word char behind';
ok  $subject ~~ /<!ww>gh/,  '<!ww> succeeds with a non-word char behind';
ok  $subject ~~ /bc<!ww>/,  '<!ww> succeeds with a non-word char ahead';
ok  $subject ~~ /<!ww>ab/,  '<!ww> succeeds at start of string';
ok  $subject ~~ /hi<!ww>/,  '<!ww> succeeds at end of string';

ok  "abc" ~~ / a <ww> bc /, 'bare <ww> is the positive assertion and zero-width';

# <?wb> is unaffected: a boundary is a *transition*, so it is false inside a
# run of word chars AND inside a run of non-word chars.
ok  $subject ~~ /<?wb>def/, '<?wb> at a non-word/word transition';
ok  $subject ~~ /abc<?wb>/, '<?wb> at a word/non-word transition';
nok $subject ~~ /a<?wb>/,   '<?wb> fails between two word chars';
nok $subject ~~ /\-<?wb>/,  '<?wb> fails between two non-word chars';
nok $subject ~~ /<!wb>def/, '<!wb> fails at a boundary';

# ---------------------------------------------------------------------------
# A custom `ws` token override, the documented `<!ww> \h*` idiom.
# ---------------------------------------------------------------------------

grammar Demo {
    token ws {
        <!ww>       # only match when not within a word
        \h*         # only match horizontal whitespace
    }
    rule TOP {
        a b '.'
    }
}

nok Demo.parse("ab."),     'custom ws with <!ww> rejects a zero-width gap inside a word';
ok  Demo.parse("a b."),    'custom ws matches a plain space';
ok  Demo.parse("a\tb ."),  'custom ws matches a tab';
nok Demo.parse("a\tb\n."), 'custom ws (\h* only) rejects a newline';

# A plain custom ws (no <!ww>) does allow the zero-width gap.
grammar DemoPlain {
    token ws { \h* }
    rule TOP { a b '.' }
}
ok DemoPlain.parse("ab."), 'a plain custom ws may match zero characters between two atoms';

# ---------------------------------------------------------------------------
# Dynamically-scoped rule parameters.
#
# `token value($*STOPPER = '"')` establishes $*STOPPER for the whole of value's
# match: its own body, any subrule it calls, and any depth below that. The
# binding is torn down when the rule returns.
# ---------------------------------------------------------------------------

my %seen;

grammar Own {
    token TOP { <value> }
    token value($*S = 'own') { { %seen<own> = $*S } . }
}
ok Own.parse('x'), 'Own parses';
is %seen<own>, 'own', 'a defaulted $* parameter is visible in its own rule body';

grammar Sub {
    token TOP { <value> }
    token value($*S = 'sub') { <char> }
    token char { { %seen<sub> = $*S } . }
}
ok Sub.parse('x'), 'Sub parses';
is %seen<sub>, 'sub', 'a defaulted $* parameter is visible in a called subrule';

grammar Deep {
    token TOP { <value> }
    token value($*S = 'deep') { <mid> }
    token mid { <char> }
    token char { { %seen<deep> = $*S } . }
}
ok Deep.parse('x'), 'Deep parses';
is %seen<deep>, 'deep', 'a defaulted $* parameter is visible two subrules down';

grammar Passed {
    token TOP { <value('Z')> }
    token value($*S) { <char> }
    token char { { %seen<passed> = $*S } . }
}
ok Passed.parse('x'), 'Passed parses';
is %seen<passed>, 'Z', 'an explicitly passed $* argument reaches a subrule';

grammar Nested {
    token TOP { <outer> }
    token outer($*S = 'outer') { <inner> <peek> }
    token inner($*S = 'inner') { { %seen<inner> = $*S } . }
    token peek { { %seen<peek> = $*S } }
}
ok Nested.parse('x'), 'Nested parses';
is %seen<inner>, 'inner', 'an inner rule shadows the outer rule binding';
is %seen<peek>,  'outer', 'the inner binding is torn down when that rule returns';

grammar RuleForm {
    rule TOP { <value> }
    rule value($*S = 'rule') { <char> }
    rule char { { %seen<rule> = $*S } . }
}
ok RuleForm.parse('x'), 'RuleForm parses';
is %seen<rule>, 'rule', '$* parameters work on a `rule` too';

grammar RegexForm {
    regex TOP { <value> }
    regex value($*S = 'regex') { <char> }
    regex char { { %seen<regex> = $*S } . }
}
ok RegexForm.parse('x'), 'RegexForm parses';
is %seen<regex>, 'regex', '$* parameters work on a `regex` too';

nok (try $*S).defined, 'the binding does not leak out of the parse';

# The XML::Grammar idiom: a code assertion in a shared subrule reads the
# parameter the caller set.
grammar Quoted {
    token TOP { <value> }
    token value($*STOPPER = '"') {
        \"
        [
        | \"
        | <char>+ \"
        ]
    }
    token char {
        <?{ $*STOPPER eq '"' }>
        <!["]> .
    }
}
is Quoted.parse('"hello"').Str, '"hello"',
   'a code assertion in a subrule reads the caller rule`s $* parameter';

# The parameter is also visible to pattern interpolation.
grammar Interp {
    token TOP { <value> }
    token value($*S = 'z') { <char> }
    token char { $*S }
}
ok  Interp.parse('z'), 'a $* parameter interpolates into a subrule`s pattern';
nok Interp.parse('q'), 'and the interpolated value really is matched';

# ---------------------------------------------------------------------------
# .parse(..., :args(...)) filling a start rule's $* parameters.
# ---------------------------------------------------------------------------

grammar Args {
   rule TOP ($*word, $*extra) { <phrase-stem><added-words> }
   rule phrase-stem { "I like" }
   rule added-words { $*word $*extra }
}

my $args-match = Args.parse("I like everything else", :args(("everything", "else")));
ok $args-match, ':args fills a start rule`s dynamic parameters';
is $args-match<added-words>.Str, 'everything else', 'and the subrule matched with them';

my $cap-match = Args.parse("I like everything else", :args(\("everything", "else")));
ok $cap-match, ':args also accepts a Capture';

grammar PlainArgs {
    rule TOP ($word) { "I like" $word }
}
ok PlainArgs.parse("I like cake", :args(("cake",))), 'ordinary :args parameters still work';

# ---------------------------------------------------------------------------
# FAILGOAL.
# ---------------------------------------------------------------------------

grammar Goal {
    token TOP { '[' ~ ']' \w+ };
    method FAILGOAL($goal) {
        die "Cannot find $goal near position {self.pos}"
    }
}

is Goal.parse('[good]').Str, '[good]', 'a satisfied goal parses normally';

my $goal-err = '';
{
    Goal.parse('[bad');
    CATCH { default { $goal-err = .Str } }
}
is $goal-err, "Cannot find ']'  near position 4",
   'FAILGOAL is invoked with the goal source text and the cursor position';

# The goal text is the source form, so it differs when no space separates the
# goal from the conjunction's content.
grammar GoalTight {
    token TOP { '[' ~ ']'\w+ };
    method FAILGOAL($goal) { die "goal={$goal.raku}" }
}
my $tight-err = '';
{
    GoalTight.parse('[bad');
    CATCH { default { $tight-err = .Str } }
}
is $tight-err, 'goal="\']\'"', 'a goal with no trailing space reports none';

# The innermost failing goal wins.
grammar GoalNested {
    token TOP { '[' ~ ']' [ '(' ~ ')' \w+ ] };
    method FAILGOAL($goal) { die "goal={$goal.raku} pos={self.pos}" }
}
my $nested-err = '';
{
    GoalNested.parse('[(bad]');
    CATCH { default { $nested-err = .Str } }
}
is $nested-err, 'goal="\')\' " pos=5', 'the inner goal is the one reported';

# A grammar with no FAILGOAL just fails to parse.
grammar NoGoal {
    token TOP { '[' ~ ']' \w+ }
}
my $no-goal-err = 'none';
{
    NoGoal.parse('[bad');
    CATCH { default { $no-goal-err = .Str } }
}
is $no-goal-err, 'none', 'a missing goal without FAILGOAL throws nothing';
