use Test;

# Code inside a separated quantifier (`atom +% sep`) or a `&` conjunction sees
# what rakudo's one cursor has there: the captures taken before the construct,
# the iterations folded so far with the one in progress folded in place, and
# the earlier conjunction branches' captures. Every expected value below is
# rakudo's.

plan 14;

sub show($m) {
    $m.list.map({ $_ ~~ Positional ?? '[' ~ .map(~*).join(',') ~ ']' !! ($_ // 'Nil').Str }).join('|')
        ~ ' /' ~ ~$m ~ '/'
}

my @log;

# A capture before the quantifier keeps its own slot.
@log = ();
"x1,2,3" ~~ / (x) [ (\d) { @log.push: show($/) } ] +% ',' /;
is @log.join(' ; '), 'x|[1] /x1/ ; x|[1,2] /x1,2/ ; x|[1,2,3] /x1,2,3/',
    'atom code sees the earlier capture and the folded iterations';

@log = ();
"x1,2" ~~ / (x) [ [ (\d) { @log.push: show($/) } ] +% ',' ] /;
is @log.join(' ; '), 'x|[1] /x1/ ; x|[1,2] /x1,2/',
    'the same inside a non-capturing group';

# Separator code sees the chain so far, its own captures folded in.
@log = ();
"x1,2,3" ~~ / (x) [ (\d) ] +% [ ',' { @log.push: show($/) } ] /;
is @log.join(' ; '), 'x|[1] /x1,/ ; x|[1,2] /x1,2,/', 'separator code sees the chain so far';

@log = ();
"x1,2,3" ~~ / (x) (\d) +% [ (',') { @log.push: show($/) } ] /;
is @log.join(' ; '), 'x|[1]|[,] /x1,/ ; x|[1,2]|[,,,] /x1,2,/',
    'a separator capture folds into the separator slot';

# The atom after a separator sees that separator.
@log = ();
"1;2;3" ~~ / [ (\d) { @log.push: show($/) } ] +% [ (';') ] /;
is @log.join(' ; '), '[1]|[] /1/ ; [1,2]|[;] /1;2/ ; [1,2,3]|[;,;] /1;2;3/',
    'atom code sees the separators matched so far';

@log = ();
"1;2;3;" ~~ / [ (\d) ] +%% [ (';') { @log.push: show($/) } ] /;
is @log.join(' ; '), '[1]|[;] /1;/ ; [1,2]|[;,;] /1;2;/ ; [1,2,3]|[;,;,;] /1;2;3;/ ; [1,2,3]|[;,;,;] /1;2;3;/',
    'a %% trailing separator sees the whole chain';

# Under ratchet too.
@log = ();
"x1,2,3" ~~ / :r (x) (\d) +% [ (',') { @log.push: show($/) } ] /;
is @log.join(' ; '), 'x|[1]|[,] /x1,/ ; x|[1,2]|[,,,] /x1,2,/', 'ratcheted separator code';

@log = ();
"1;2;3" ~~ / :r [ (\d) { @log.push: show($/) } ] +% [ (';') ] /;
is @log.join(' ; '), '[1]|[] /1/ ; [1,2]|[;] /1;2/ ; [1,2,3]|[;,;] /1;2;3/', 'ratcheted atom code';

# A code assertion that fails an iteration backtracks into the atom.
@log = ();
"12,345,6" ~~ / ^ [ (\d+) <?{ @log.push: show($/); $/[0][*-1].chars < 3 }> ] +% ',' /;
is @log.join(' ; '), '[12] /12/ ; [12,345] /12,345/ ; [12,34] /12,34/',
    'a rejected iteration is retried shorter';
is show($/), '[12,34] /12,34/', 'and the match keeps the accepted iterations';

# A `** {…}` count reads the iteration's own capture.
is show("2ab,1c" ~~ / [ (\d) \w ** {+$/[0][*-1]} ] +% ',' /), '[2,1] /2ab,1c/',
    'a repeat count reads the folded iterations';

# Conjunction: every branch sees the enclosing captures, later ones the
# earlier branches' too.
@log = ();
"ab" ~~ / (a) [ (\w) { @log.push: 'D1 ' ~ show($/) } & \w { @log.push: 'D2 ' ~ show($/) ~ ' ' ~ ~$0 } ] /;
is @log.join(' ; '), 'D1 a|b /ab/ ; D2 a|b /ab/ a', 'conjunction branches see the enclosing and earlier captures';

is ~("aa" ~~ / :my $x = 'a'; [ \w & $x ] $x /), 'aa', 'a later branch reads a :my lexical';

@log = ();
"abc" ~~ / :my $n = 2; (a) [ \w ** {$n} & (\w\w) { @log.push: show($/) } ] /;
is @log.join(' ; '), 'a|bc /abc/', 'a later branch with a capture and a count';
