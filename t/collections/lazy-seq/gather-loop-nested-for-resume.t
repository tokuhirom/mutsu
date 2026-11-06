use Test;
plan 6;

# A `for` nested in a condition-driven loop inside a lazily pulled gather: a
# take-limit suspension in the `for` must resume the same loop iteration.

sub inn($n) { gather { take "a$n"; take "b$n" } }

sub o-loop() {
    gather loop (my $k = 1; $k <= 2; $k++) { for inn($k) -> $x { take "$k:$x" } }
}
is o-loop().map({ $_ }).join(" "), "1:a1 1:b1 2:a2 2:b2", 'loop { for lazy-gather }';

sub o-list() {
    gather loop (my $k = 1; $k <= 2; $k++) { for (1, 2) -> $x { take "$k:$x" } }
}
is o-list().map({ $_ }).join(" "), "1:1 1:2 2:1 2:2", 'loop { for list }';

my $c = 0;
my $g = gather while $c++ < 3 { for inn($c) -> $x { take "$c:$x" } };
is $g.map({ $_ }).join(" "), "1:a1 1:b1 2:a2 2:b2 3:a3 3:b3",
    'while with a side-effecting condition runs it once per iteration';

my $d = 0;
my $h = gather until $d++ >= 2 { for inn($d) -> $x { take "$d:$x" } };
is $h.map({ $_ }).join(" "), "1:a1 1:b1 2:a2 2:b2", 'until';

# The recursive shape of Math::Sequences' strict-partitions.
sub sp(Int:D $n, Int :$target = $n) {
    gather loop (my $i = min($n, $target); $i > 0; $i--) {
        given $target - $i -> $remain {
            when $remain == 0 { take [$i] }
            default {
                for sp($i - 1, :target($remain)) -> $rest { take [$i, |$rest] }
            }
        }
    }
}
is sp(5).map(*.join(" ")).sort.join(" | "), "3 2 | 4 1 | 5", 'recursive lazy partitions';
is sp(9).map(*.join(" ")).sort.join(" | "),
    "4 3 2 | 5 3 1 | 5 4 | 6 2 1 | 6 3 | 7 2 | 8 1 | 9", 'recursive lazy partitions, 9';
