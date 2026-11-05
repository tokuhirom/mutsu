use Test;

plan 5;

# A literal in a named parameter's sub-signature takes part in candidate
# selection: `:@op ('in', ...)` must not accept '<'.
multi ec(:@op ('in', $l, *@values)) { "in" }
multi ec(:@op ('not-in', $l, *@values)) { "not-in" }
multi ec(:op(@) (Str $key, $l, $r)) { "bin" }

is ec(|\(:op['<', 1, 2])), 'bin', "a different literal falls through to the later candidate";
is ec(|\(:op['in', 1, 2, 3])), 'in', 'the matching literal selects its candidate';
is ec(|\(:op['not-in', 1, 2])), 'not-in', 'a second literal candidate';
is ec(|\(:op['<', 1, \(:and[1, 2])])), 'bin', 'a Capture operand does not change the choice';

sub one(:@op ('x', $a)) { "x:$a" }
is one(|\(:op['x', 5])), 'x:5', 'a single candidate with a literal still binds';
