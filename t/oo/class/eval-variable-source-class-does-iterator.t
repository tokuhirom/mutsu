use Test;
use MONKEY-SEE-NO-EVAL;

plan 4;

# A class that `does Iterator` gets the role's default methods (`push-exactly`,
# `push-all`, ...) in code that is EVAL'd from a string held in a variable. The
# program text never mentions the composition, so the role's defaults must be
# provided for the EVAL'd unit itself (#16387).
my $src = 'class EvalIter { has $.n = 0; method pull-one { $!n++ < 3 ?? $!n !! IterationEnd } }'
        ~ "\n" ~ 'EvalIter';
my $src2 = $src.subst('class EvalIter {', 'class EvalIter does Iterator {');
my $class = EVAL $src2;

my @got;
my $it = $class.new;
ok $it.push-all(@got) =:= IterationEnd, 'push-all answers IterationEnd';
is-deeply @got, [1, 2, 3], 'push-all pulled every value';

my @two;
ok $class.new.push-exactly(@two, 2) == 2, 'push-exactly stops early with the count';
is-deeply @two, [1, 2], 'push-exactly pulled two values';
