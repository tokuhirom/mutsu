use Test;

# A prefix read of a deferred `.grep` pulls its source a bit at a time for
# every callback shape: a FIRST phaser fires once for the Seq, and a
# multi-parameter block takes whole groups, as under Rakudo. Such a grep used
# to drain its whole source on the first read (#14196). Expected values are
# the `raku` (v2026.09) output.

plan 12;

my ($n, $f) = 0, 0;
my $s = (1..10).grep({ FIRST { $f++ }; $n++; $_ %% 2 });
is $s[0], 2, 'a prefix read of a grep with a FIRST phaser';
is "$n $f", "2 1", '... runs the callback twice and FIRST once';
is $s[1], 4, 'a later read of the same Seq';
is "$n $f", "4 1", '... fires FIRST no second time';
is $s.elems, 5, 'elems reaches the end';
is "$n $f", "10 1", '... having run the callback over every element once';

my $m = 0;
my $t = (1..10).grep(-> $a, $b { $m++; $a %% 3 });
is $t[0].raku, '(3, 4)', 'a two-parameter grep matches a group';
is $m, 2, '... and ran the callback over two groups only';
is $t[1].raku, '(9, 10)', 'a later group';
is $m, 5, '... after running the groups in between';

my $u = (1..5).grep(-> $a, $b { True });
throws-like { $u.elems }, Exception, message => /'Too few positionals'/,
    'a short final group is an error';

my $k = 0;
my $c = (1..20).grep(-> $a, *@r { $k++; True });
is $c[1], 2, 'a grep with a slurpy parameter takes one element per call';
