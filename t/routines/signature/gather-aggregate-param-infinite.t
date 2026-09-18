use v6;
use Test;

plan 2;

sub grep-div(@a, Int:D $n) {
    gather for @a {
        take $_ if $_ %% $n;
    }
}

my \evens = grep-div((1...*), 2);
is grep-div(evens, 3)[^4].join(','), '6,12,18,24',
    'a non-slurpy aggregate parameter keeps an infinite gather pullable';

sub gather-map(@x) {
    @x.map({ $_ * 2 }).join(',')
}

is gather-map(gather { take 1; take 2 }), '2,4',
    'a finite gather passed to an aggregate parameter is a List';
