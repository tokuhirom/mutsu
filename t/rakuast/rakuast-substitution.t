use Test;

# Substitutions and transliterations in RakuAST, measured on rakudo 2026.09:
#
# - `s/a/b/` is a `Substitution` whose `pattern` is the regex tree itself (no
#   `QuotedRegex` around it) and whose `replacement` is a `QuotedString`; `S///` is
#   the same with `immutable => True`, and `ss///` with `samespace => True`;
# - its adverbs are colonpairs in the order they were written: `ColonPair::True`
#   for a flag, `ColonPair::Number` for `:2nth`, `ColonPair::Value` for `:x(2)`;
# - `s[a] = EXPR` is the same with an `infix` (an `Assignment`) and the
#   expression as the `replacement`;
# - `tr/a/b/` is a `Transliteration` with the two sides as `QuotedString`s;
#   `TR///` is `destructive => False`;
# - `$0` is a `Var::PositionalCapture`, also inside a string.
#
# The round trip is the parsed program. The tree part of this file also passes
# under `raku`; the round trip part is mutsu's.

plan 63;

sub exprs($src) { ('my ($x, $s); ' ~ $src).AST.statements.skip(1).map(*.expression) }
sub same($src, $expected, $desc) {
    my $parsed = do { my @*LOG; EVAL($src) };
    my $round = do { my @*LOG; EVAL($src.AST) };
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- the tree of a substitution
{
    my $s = exprs(Q[s/a/b/])[0];
    isa-ok $s, RakuAST::Substitution, '`s///` is a substitution';
    is $s.immutable, False, 'that is not immutable';
    is $s.samespace, False, 'and not `ss`';
    isa-ok $s.pattern, RakuAST::Regex::Literal, 'its pattern is the regex tree itself';
    isa-ok $s.replacement, RakuAST::QuotedString, 'its replacement a quoted string';
    is $s.adverbs.elems, 0, 'without adverbs';
    is exprs(Q[S/a/b/])[0].immutable, True, '`S///` is immutable';
    is exprs(Q[ss/a/b/])[0].samespace, True, '`ss///` has `samespace`';
    is exprs(Q[ss/a/b/])[0].adverbs.elems, 0, 'and no adverb';
    is exprs(Q[s:ss/a/b/])[0].samespace, False, 'but `s:ss///` does not';
    is exprs(Q[s:ss/a/b/])[0].adverbs[0].key, 'ss', 'it has the adverb';
}

# --- adverbs
{
    my $a = exprs(Q[s:g:i/a/b/])[0].adverbs;
    is $a.elems, 2, 'two adverbs';
    isa-ok $a[0], RakuAST::ColonPair::True, 'a flag is a true colonpair';
    is "{$a[0].key} {$a[1].key}", 'g i', 'in the order written';
    is exprs(Q[s:i:g/a/b/])[0].adverbs.map(*.key).join(' '), 'i g', 'whichever it is';
    my $n = exprs(Q[s:2nth/a/b/])[0].adverbs[0];
    isa-ok $n, RakuAST::ColonPair::Number, '`:2nth` is a number colonpair';
    is $n.key, 'nth', 'named `nth`';
    is $n.value.value, 2, 'with its count';
    my $x = exprs(Q[s:x(2)/a/b/])[0].adverbs[0];
    isa-ok $x, RakuAST::ColonPair::Value, '`:x(2)` is a value colonpair';
    isa-ok $x.value, RakuAST::Circumfix::Parentheses, 'over a parenthesis';
    my $r = exprs(Q[s:x(1..3)/a/b/])[0].adverbs[0].value.semilist.statements[0].expression;
    isa-ok $r, RakuAST::ApplyInfix, 'whose argument may be a range';
    my $l = exprs(Q[s:nth(1,3)/a/b/])[0].adverbs[0].value.semilist.statements[0].expression;
    isa-ok $l, RakuAST::ApplyListInfix, 'or a list';
    is exprs(Q[S:g/a/b/])[0].adverbs[0].key, 'g', '`S///` takes them as well';
}

# --- the replacement
{
    my $v = exprs(Q[s/a/$x/])[0].replacement;
    isa-ok $v.segments[0], RakuAST::Var::Lexical, 'an interpolated variable is a segment';
    my $c = exprs(Q[s/(a)/<$0>/])[0].replacement;
    isa-ok $c.segments[1], RakuAST::Var::PositionalCapture, '`$0` is a positional capture';
    is $c.segments[1].index, 0, 'with its index';
    isa-ok exprs(Q[s/a/{ $x }/])[0].replacement.segments[0], RakuAST::Block, 'a code block is a block';
    my $a = exprs(Q[s[a] = $x + 1])[0];
    isa-ok $a.infix, RakuAST::Assignment, 'an assignment form has an `Assignment` infix';
    isa-ok $a.replacement, RakuAST::ApplyInfix, 'and the expression as its replacement';
    isa-ok exprs(Q[$s ~~ s/a/b/])[0].right, RakuAST::Substitution, 'a smartmatch against one';
    isa-ok exprs(Q[$0])[0], RakuAST::Var::PositionalCapture, 'a bare `$0` is a positional capture';
    is exprs(Q[$2])[0].index, 2, 'with its number';
}

# --- transliteration
{
    my $t = exprs(Q[tr/a/b/])[0];
    isa-ok $t, RakuAST::Transliteration, '`tr///` is a transliteration';
    is $t.destructive, True, 'that is destructive';
    isa-ok $t.left, RakuAST::QuotedString, 'with a quoted string on the left';
    is $t.right.segments[0].value, 'b', 'and its replacement on the right';
    is $t.adverbs.elems, 0, 'without adverbs';
    is exprs(Q[TR/a/b/])[0].destructive, False, '`TR///` is not';
    is exprs(Q[tr/a..c/x..z/])[0].left.segments[0].value, 'a..c', 'a range stays as written';
    is exprs(Q[tr:d:c/a//])[0].adverbs.map(*.key).join(' '), 'd c', 'adverbs in the order written';
    is exprs(Q[tr:s:d/a//])[0].adverbs.map(*.key).join(' '), 's d', 'whichever it is';
}

# --- the round trip is the parsed program
same Q[my $s = "abc"; $s ~~ s/b/X/; $s], 'aXc', 'a substitution';
same Q[my $s = "abab"; $s ~~ s:g/a/X/; $s], 'XbXb', 'a global one';
same Q[my $s = "aAbB"; $s ~~ s:g:i/a/X/; $s], 'XXbB', 'a global, case-insensitive one';
same Q[my $s = "aaaa"; $s ~~ s:2nth/a/X/; $s], 'aXaa', 'the second match';
same Q[my $s = "aaaa"; $s ~~ s:nth(1,3)/a/X/; $s], 'XaXa', 'the first and third';
same Q[my $s = "aaaa"; $s ~~ s:x(2)/a/X/; $s], 'XXaa', 'twice';
same Q[my $s = "aaaaa"; $s ~~ s:g:x(1..3)/a/X/; $s], 'XXXaa', 'a range of times';
same Q[my $s = "abc"; my $t = do given $s { S/b/X/ }; "$s $t"], 'abc aXc', 'a non-destructive substitution';
same Q[my $s = "abab"; my $t = do given $s { S:g/a/X/ }; "$s $t"], 'abab XbXb', 'a global non-destructive one';
same Q[my $s = "abcd"; $s ~~ s/(\w)(\w)/$1$0/; $s], 'bacd', 'positional captures in the replacement';
same Q[my $s = "ab"; $s ~~ s/$<x>=(a)/<$<x>>/; $s], '<a>b', 'a named capture in the replacement';
same Q[my $x = 3; my $s = "a"; $s ~~ s/a/{ $x * 2 }/; $s], '6', 'a code block in the replacement';
same Q[my $x = 3; my $s = "a"; $s ~~ s/a/v$x/; $s], 'v3', 'an interpolated variable';
same Q[my $s = "abab"; $s ~~ s:g[(a)] = $0 x 2; $s], 'aabaab', 'the assignment form';
same Q[$_ = "xax"; s/a/b/; $_], 'xbx', 'against the topic';
same Q[my $s = "Foo foo"; $s ~~ s:g:ii/foo/bar/; $s], 'Bar bar', 'a samecase substitution';
same Q[my $s = "a  b"; $s ~~ ss/a b/c d/; $s], 'c  d', 'a samespace substitution';
same Q[my $s = "banana"; $s ~~ tr/a..b/A..B/; $s], 'BAnAnA', 'a transliteration';
same Q[my $s = "banana"; $s ~~ tr:d/a//; $s], 'bnn', 'a deleting one';
same Q[my $s = "banana"; $s ~~ tr:c/a//; $s], 'aaa', 'a complementing one';
same Q[my $s = "baaanaa"; $s ~~ tr:s/a/a/; $s], 'bana', 'a squashing one';
same Q[my $s = "banana"; my $t = do given $s { TR/a/o/ }; "$s $t"], 'banana bonono', 'a non-destructive transliteration';
