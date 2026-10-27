use Test;

plan 6;

# A lazily pulled `.map` with a multi-positional callback hands it chunks of
# `arity` elements. The final chunk may be short: the callback still runs when
# the elements left cover its REQUIRED positionals (`$b?`, `$b = 9`), and
# dies only when they do not. Eager lists always did this; a Seq over an
# iterator (what Red's `ResultSeq.Seq` is) died with "Not enough elements".
class It does Iterator {
    has Int $.i is rw = 0;
    has Int $.n is required;
    method pull-one { $!i < $!n ?? ++$!i !! IterationEnd }
}

sub seq($n) { Seq.new(It.new(:$n)) }

is seq(5).map(-> $a, $b? { "{$a}-{$b // 'U'}" }).join(' '), '1-2 3-4 5-U',
    'optional second positional, odd element count';
is seq(4).map(-> $a, $b? { "{$a}-{$b // 'U'}" }).join(' '), '1-2 3-4',
    'even element count';
is seq(5).map(-> $a, $b = 9 { "{$a}-{$b}" }).join(' '), '1-2 3-4 5-9',
    'a defaulted second positional';
is seq(5).map(-> $a, $b?, $c? { "{$a}-{$b // 'U'}-{$c // 'U'}" }).join(' '), '1-2-3 4-5-U',
    'two optional positionals, a final chunk of two';
is (1, 2, 3).map(-> $a, $b? { "{$a}-{$b // 'U'}" }).join(' '), '1-2 3-U',
    'the eager list agrees';
throws-like { seq(5).map(-> $a, $b { "{$a}-{$b}" }).join(' ') }, Exception,
    'a required second positional with one element left still dies';
