use v6;
use Test;

# Lazy pulls from a gather whose take sits inside a condition-driven loop
# (`while`/`loop`) suspend at the loop's iteration boundary, so the resumed
# iteration neither replays nor drops the statements after the take
# (99problems-31-to-40.t P37: `while $n > 1 { take $n; $n div= 2 }` yielded
# 6,6,6... on indexed pulls). Also: a lazily-forced gather body runs under
# its own readonly context, not the consumer frame's.

plan 6;

{
    sub halves(Int $n is copy) {
        gather { while $n > 1 { take $n; $n div= 2; } }
    }
    my $s = halves(6);
    is $s[0], 6, 'first pulled element';
    is $s[1], 3, 'second pulled element continues the loop state';
    is-deeply $s.List, (6, 3), 'full force matches the pulls';
}

{
    # for-loop iteration over the lazy while-gather (the P37 shape).
    sub pfm(Int $n is copy) {
        gather {
            my $cond = 2;
            my $count = 0;
            while $n > 1 {
                if $n % $cond == 0 { $count++; $n div= $cond; }
                else {
                    if $count > 0 { take [$cond, $count]; $count = 0; }
                    $cond++;
                }
            }
            take [$cond, $count];
        }
    }
    my @got;
    for pfm(12) -> @a { @got.push(@a.join('^')) }
    is @got.join(' '), '2^2 3^1', 'iterating a lazy while-gather yields each factor once';
}

{
    # The gather body's `is copy` param stays writable when forced inside a
    # consumer routine with a same-named readonly param.
    sub gen(Int $n is copy) {
        gather { while $n > 0 { take $n; $n--; } }
    }
    sub consume($n) {
        my @out;
        for gen($n) -> $x { @out.push($x) }
        @out.join(',');
    }
    is consume(3), '3,2,1', 'lazy body write is not blocked by consumer readonly marks';
}

{
    # for-shaped gather bodies keep the at-take suspension (roast
    # S04-statements/gather.t "gather is lazy" pins the side-effect timing).
    my $count = 0;
    my @list := gather {
        for 1 .. 10 -> $a {
            take $a;
            $count++;
        }
    }.List;
    my $ = @list[2];
    is $count, 2, 'for-shaped gather still suspends at the take';
}
