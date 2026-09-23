use Test;

# A zero-width atom under a counted quantifier (`**N`) counts every
# iteration: each one matches the empty string at the same position, so the
# quantifier reaches its count and the match succeeds (#9180). A bounded
# quantifier repeats up to its maximum, as Rakudo does.

plan 14;

ok  ("xx" ~~ /<?before x>**2/).so,      '<?before x>**2 matches';
ok  ("ab" ~~ /<?before a>**1 a/).so,    '<?before a>**1 then a literal';
ok  ("xx" ~~ /<?before x>**2..3 x/).so, 'a range count over a lookahead';
ok  ("xx" ~~ /[<?before x>]**2 x/).so,  'a bracketed lookahead';
ok  ("xx" ~~ /:r <?before x>**2 x/).so, 'under :ratchet';
ok  ("xx" ~~ /x**0..2 <?before x>**3/).so, 'backtracks the preceding quantifier';
nok ("xx" ~~ /<?before y>**2/).so,      'a failing lookahead still fails';

is-deeply ("xx" ~~ /<before x>**2/).keys.List, ('before',), 'capturing <before> under **2 records the capture';
is ("xx" ~~ /<before x>**2/)<before>.elems, 2, 'one <before> capture per iteration';
is ("xx" ~~ /<before x>**3..5 x/)<before>.elems, 5, 'a bounded range repeats up to its maximum';
is ("xx" ~~ /(<?before x>)**2 x/)[0].elems, 2, 'a positional group under **2 records each iteration';
is ("ab" ~~ /[a|<?before b>]**2 b/).Str, 'ab', 'a zero-width alternative can fill the count';

# `.so` / `.not` on a successful zero-width Match answer from the Match, not
# from its (empty) matched string.
my $m = "yy" ~~ /x?/;
ok  $m.so,  '.so on an empty successful Match is True';
nok $m.not, '.not on an empty successful Match is False';
