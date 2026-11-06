use Test;

# From Sequence::Generator: a class that composes only PredictiveIterator gets
# the Iterator defaults (`skip-one`, `skip-at-least`, ...) through it.
plan 6;

class P does PredictiveIterator {
    has $.i = 0;
    method pull-one { $!i < 3 ?? $!i++ !! IterationEnd }
    method count-only { 3 - $!i }
}

is P.new.skip-one, 1, 'skip-one';
is P.new.skip-at-least(2), 1, 'skip-at-least';
is P.new.skip-at-least(5), 0, 'skip-at-least past the end';
ok P.new.bool-only, 'bool-only';
ok P.new ~~ Iterator, 'it is an Iterator';
ok P.new ~~ PredictiveIterator, 'and a PredictiveIterator';
