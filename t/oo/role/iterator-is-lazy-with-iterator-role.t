use Test;

plan 3;

# A program that composes the Iterator role gets the role's `is-lazy` default
# (False); a built-in iterator still reports its own laziness.
class Counter does Iterator {
    has $.n = 0;
    method pull-one { $!n < 3 ?? ++$!n !! IterationEnd }
}

my $lazy = [4, 5, 6].lazy.iterator;
ok $lazy.is-lazy, 'a built-in iterator over a lazy array is lazy';
nok [4, 5, 6].iterator.is-lazy, 'one over an eager array is not';
nok Counter.new.is-lazy, "and the composing class gets the role's default";
