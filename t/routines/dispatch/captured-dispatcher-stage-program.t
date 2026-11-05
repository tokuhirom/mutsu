use Test;
use lib 't/lib';
use CapturedStageProgram;

# A captured dispatcher (an imported `my proto`) answers the candidate checks
# the argument-type key decides once per key (perf) and runs only the
# value-dependent ones per call; winners and errors stay those of the full walk.
plan 13;

is put-in(1, 2), 'int:no', 'typed positional candidate';
is put-in(1, 2, :force), 'int:True', 'a named argument selects through the program';
is put-in(1, 2, :force(7)), 'int:7', 'and carries its value';
is put-in('a', 'b'), 'str', 'a Str candidate';
is put-in('a', 2), 'cmp-force:no', 'the untyped candidate with optional named params';
is put-in('a', 2, 3, 4), 'also(2):no', 'a slurpy candidate takes the extra positionals';
is put-in('a', 2, 3, :force), 'also(1):True', 'slurpy plus a named argument';
is put-in(notfound, 5, :pos(notfound)), 'pos', 'a required named parameter with its own type';

# The same call shapes repeated: the cached program gives the same answers.
for ^3 {
    is put-in(1, 2, :force), 'int:True', "repeat $_ of a named-argument call" if $_ == 2;
}

# A where-constrained array candidate keeps its value check.
my @short = 1, 2;
my @long = 1, 2, 3, 4;
is put-in(@short, 0), 'array', 'a short array fails the where clause';
is put-in(@long, 0), 'long-array', 'a long array passes it';
is put-in(@short, 0), 'array', 'and the short one again';
dies-ok { EVAL 'put-in()' }, 'no candidate for no arguments';
