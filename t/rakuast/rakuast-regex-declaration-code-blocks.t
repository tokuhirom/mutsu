use Test;

# A regex declaration's code runs after the round trip through RakuAST (#11923). The
# matcher reads a pattern's text, and the tree holds a code block only as statements, so
# the converter keeps each block's spelling in a hidden `source` field and lowering puts it
# back; a hand-built node has none and lowers to an empty block.

plan 14;

sub same($src, $expected, $desc) {
    my $parsed = do { my @*LOG; EVAL($src) };
    my $round = do { my @*LOG; EVAL($src.AST) };
    is "$parsed|$round", "$expected|$expected", $desc;
}

same Q[my $n = 0; grammar G1 { token TOP { a { $n += 10 } } }; G1.parse("a"); $n], 10,
    'a code block in a grammar token runs';
same Q[my $n = 0; my token t { b { $n += 100 } }; "b" ~~ /<t>/; $n], 100,
    'a code block in a lexical token runs';
same Q[my $n = 0; my regex r { c { $n++ } d { $n += 2 } }; "cd" ~~ /<r>/; $n], 3,
    'several blocks run in order';
same Q[my @seen; grammar G2 { token TOP { <w> { @seen.push(~$<w>) } }; token w { \w+ } }; G2.parse("ab"); @seen.join(",")], 'ab',
    'a block reads the match so far';
same Q[grammar G3 { token TOP { <?{ 1 }> a }; }; ~G3.parse("a")], 'a',
    'a true predicate block passes';
same Q[grammar G4 { token TOP { [ <!{ 1 }> a | b ] }; }; ~G4.parse("b")], 'b',
    'a false negated predicate block falls through';
same Q[my $re = "a"; grammar G5 { token TOP { <{ "a" }> b }; }; ~G5.parse("ab")], 'ab',
    'an interpolated block supplies a pattern';
same Q[my $n = 0; grammar G6 { token TOP { a ** { 2 } { $n++ } }; }; G6.parse("aa"); $n], 1,
    'a block range and a block in one token';
same Q[grammar G7 { token TOP { :my $x = 5; a { $*r = $x } }; }; my $*r; G7.parse("a"); $*r], 5,
    'a `:my` statement declares for the blocks after it';
same Q[my $n = 0; grammar G8 { rule TOP { a { $n++ } b { $n++ } } }; G8.parse("a b"); $n], 2,
    'blocks in a rule';
same Q[my $n = 0; my $m = "xay" ~~ / a { $n = 7 } /; "$m $n"], 'a 7',
    'a code block in a quoted regex';
same Q[my $n = 0; my token t { :my $k = 4; { $n = $k } a }; "a" ~~ /<t>/; $n], 4,
    'a statement then a block in a lexical token';
same Q[my $n = 0; grammar G9 { token TOP { a { $n += 1 } | b { $n += 2 } } }; G9.parse("b"); $n], 2,
    'a block in the branch that matched';
same Q[my $n = 0; grammar G10 { token TOP { [ a { $n++ } ]+ } }; G10.parse("aaa"); $n], 3,
    'a block in a repeated group';
