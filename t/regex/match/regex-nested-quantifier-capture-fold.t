use Test;

# A capture group under nested quantifiers is ONE slot in raku: the groups
# around it do not capture, so every iteration of every level lands in the same
# flat list, in the match's final value and in what code inside an iteration
# sees. Every expected value below is rakudo's.

plan 30;

sub show($m) {
    $m.list.map({ $_ ~~ Positional ?? '[' ~ .map(~*).join(',') ~ ']' !! ($_ // 'Nil').Str }).join('|')
        ~ ' /' ~ ~$m ~ '/'
}

# --- the match's value: an inner quantifier's list is flattened, not
# --- collapsed to its last entry.
is show("1.2;3.4" ~~ / [ [ (\d) ] +% '.' ] +% ';' /), '[1,2,3,4] /1.2;3.4/',
    'separated in separated';
is show("123;45" ~~ / [ [ (\d) ]+ ] +% ';' /), '[1,2,3,4,5] /123;45/',
    'plain in separated';
is show("1.2.3.4" ~~ / [ [ (\d) ] +% '.' ]+ /), '[1,2,3,4] /1.2.3.4/',
    'separated in plain';
is show("1234" ~~ / [ [ (\d) ]+ ]+ /), '[1,2,3,4] /1234/', 'plain in plain';
is show("1.2;3.4;5" ~~ / :r [ [ (\d) ] +% '.' ] +% ';' /), '[1,2,3,4,5] /1.2;3.4;5/',
    'ratcheted';
is show("a1b2" ~~ / [ [ (<alpha>) (<digit>) ]+ ]+ /), '[a,b]|[1,2] /a1b2/',
    'two groups, each flat';
is show("a1.b2;c3" ~~ / [ [ (<alpha>) (<digit>) ] +% '.' ] +% ';' /), '[a,b,c]|[1,2,3] /a1.b2;c3/',
    'two groups under separated quantifiers';
is show("aab" ~~ / [ (a)* b ]+ /), '[a,a] /aab/', 'a star inside a plain quantifier';
is show("1.2;3.4" ~~ / [ [ (\d) ] +% '.' ] +% (';') /), '[1,2,3,4]|[;] /1.2;3.4/',
    'the outer separator keeps its own slot';

# A capture group's own quantified sub-captures stay inside its Match.
{
    my $m = "1.2;3.4" ~~ / ( (\d) +% '.' ) +% ';' /;
    is $m[0].elems, 2, 'a capturing group per outer iteration';
    is $m[0][0][0].map(~*).join(','), '1,2', 'the first one holds its own list';
    is $m[0][1][0].map(~*).join(','), '3,4', 'the second one holds its own';
}

# An inner separator's captures take slots after the atom's, flat as well.
is show("1.2;3.4" ~~ / [ [ (\d) ] +% (<[.]>) ] +% ';' /), '[1,2,3,4]|[.,.] /1.2;3.4/',
    'inner separator capture, separated outer';
is show("1.2;3.4" ~~ / [ [ (\d) ] +% (<[.]>) ]+ % ';' /), '[1,2,3,4]|[.,.] /1.2;3.4/',
    'inner separator capture, plain outer';
is show("1.2.3" ~~ / [ (\d) +% (<[.]>) ]+ /), '[1,2,3]|[.,.] /1.2.3/',
    'a separator capture under a plain quantifier takes its own slot';
is show("1.2;3.4" ~~ / [ [ (\d) ] +% (<[.]>) ] +% (';') /), '[1,2,3,4]|[.,.]|[;] /1.2;3.4/',
    'inner and outer separator captures';

# --- what code inside an iteration sees.
my @log;

@log = ();
"1.2;3.4" ~~ / [ [ (\d) { @log.push: show($/) } ] +% '.' ] +% ';' /;
is @log.join(' ; '), '[1] /1/ ; [1,2] /1.2/ ; [1,2,3] /1.2;3/ ; [1,2,3,4] /1.2;3.4/',
    'the inner iteration is folded into the outer atom slot';

@log = ();
"1.2;3.4" ~~ / :r [ [ (\d) { @log.push: show($/) } ] +% '.' ] +% ';' /;
is @log.join(' ; '), '[1] /1/ ; [1,2] /1.2/ ; [1,2,3] /1.2;3/ ; [1,2,3,4] /1.2;3.4/',
    'the same under ratchet';

@log = ();
"1.2.3" ~~ / [ [ (\d) { @log.push: show($/) } ] +% '.' ]+ /;
is @log.join(' ; '), '[1] /1/ ; [1,2] /1.2/ ; [1,2,3] /1.2.3/', 'inside a plain outer quantifier';

@log = ();
"1.2;3.4/5" ~~ / [ [ [ (\d) { @log.push: show($/) } ] +% '.' ] +% ';' ] +% '/' /;
is @log.join(' ; '), '[1] /1/ ; [1,2] /1.2/ ; [1,2,3] /1.2;3/ ; [1,2,3,4] /1.2;3.4/ ; [1,2,3,4,5] /1.2;3.4/5/',
    'three levels deep';

@log = ();
"a1.b2;c3" ~~ / [ [ (<alpha>) (<digit>) { @log.push: show($/) } ] +% '.' ] +% ';' /;
is @log.join(' ; '), '[a]|[1] /a1/ ; [a,b]|[1,2] /a1.b2/ ; [a,b,c]|[1,2,3] /a1.b2;c3/',
    'two groups in the inner atom';

@log = ();
"x1.2;3" ~~ / (x) [ [ (\d) { @log.push: show($/) } ] +% '.' ] +% ';' /;
is @log.join(' ; '), 'x|[1] /x1/ ; x|[1,2] /x1.2/ ; x|[1,2,3] /x1.2;3/',
    'a capture before the outer quantifier keeps its slot';

@log = ();
"1.2;3.4" ~~ / [ [ (\d) { @log.push: show($/) } ] +% (<[.]>) ] +% ';' /;
is @log.join(' ; '), '[1]|[] /1/ ; [1,2]|[.] /1.2/ ; [1,2,3]|[.] /1.2;3/ ; [1,2,3,4]|[.,.] /1.2;3.4/',
    'the inner separator slot is flat across outer iterations';

# Code in the outer atom, after the inner quantifier, sees it complete.
@log = ();
"1.2;3.4" ~~ / [ [ (\d) ] +% '.' { @log.push: show($/) } ] +% ';' /;
is @log.join(' ; '), '[1,2] /1.2/ ; [1,2,3,4] /1.2;3.4/', 'code after the inner quantifier';

@log = ();
"1.2;3.4" ~~ / [ (\d) +% '.' { @log.push: show($/) } ] +% ';' /;
is @log.join(' ; '), '[1,2] /1.2/ ; [1,2,3,4] /1.2;3.4/',
    'the inner quantifier is the outer atom\'s own token';

@log = ();
"1.2;3.4" ~~ / [ (\d) +% '.' { @log.push: show($/) } ]+ % ';' /;
is @log.join(' ; '), '[1,2] /1.2/ ; [1,2,3,4] /1.2;3.4/', 'and under a plain outer quantifier';

# Code in the separators.
@log = ();
"1.2;3.4" ~~ / [ [ (\d) ] +% [ '.' { @log.push: show($/) } ] ] +% ';' /;
is @log.join(' ; '), '[1] /1./ ; [1,2,3] /1.2;3./', 'inner separator code';

@log = ();
"1.2;3.4" ~~ / [ [ (\d) ] +% '.' ] +% [ ';' { @log.push: show($/) } ] /;
is @log.join(' ; '), '[1,2] /1.2;/', 'outer separator code';

# Same-level quantifiers are unchanged.
@log = ();
"x1,2,3" ~~ / (x) [ (\d) { @log.push: show($/) } ] +% ',' /;
is @log.join(' ; '), 'x|[1] /x1/ ; x|[1,2] /x1,2/ ; x|[1,2,3] /x1,2,3/',
    'a single level still folds in place';

is show("1,2,3" ~~ / [ (\d) ] +% ',' /), '[1,2,3] /1,2,3/', 'and its value is the same';
