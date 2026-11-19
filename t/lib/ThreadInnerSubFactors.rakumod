# Fixture for t/modules/start-module-inner-sub.t: a module routine whose
# block declares the inner subs it calls (one of them named like an outer
# routine), read through a lazy module array from a `start` block.
unit module ThreadInnerSubFactors;

sub factors($n) {
    gather do {
        if $n < 4 {
            take $n;
        }
        else {
            .take for split-off($n);
            sub split-off(Int $n) {
                return $n if $n.is-prime;
                my $factor = find-factor($n);
                sort flat split-off($factor), split-off($n div $factor);
            }
            sub find-factor(Int $n) {
                return 2 unless $n +& 1;
                3
            }
        }
    }
}

sub split-off($n) { factors($n).grep: { $^d >= 2 } }

our @COUNTS = (0..*).map: -> $n { split-off($n).elems };
