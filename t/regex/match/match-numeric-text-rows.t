use Test;

# Match's Int, Num, Numeric, chars, not, WHICH and replace-with are rows of the
# method table (ADR-11276 section 9.59).

plan 10;

my $m = "42abc" ~~ /\d+/;
is $m.Int, 42, 'Match.Int is the matched text as an Int';
is $m.Num, 42e0, 'Match.Num';
is $m.Numeric, 42, 'Match.Numeric';
is $m.chars, 2, 'Match.chars counts the matched text';
is $m.not, False, 'a successful match is not "not"';
is ("abc" ~~ /x/).not, True, 'a failed match is';
ok $m.WHICH.Str.starts-with('Match|'), 'Match.WHICH is an identity';

my $w = "hello world" ~~ /wor/;
is $w.replace-with("XX"), 'hello XXld', 'Match.replace-with';
is ("abc" ~~ /x/).replace-with("Y").raku, 'Nil', 'a failed match replaces nothing';
ok ("abc" ~~ /b/).Int ~~ Failure, 'non-numeric text gives a Failure';
