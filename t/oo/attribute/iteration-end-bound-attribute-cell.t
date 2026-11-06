use v6;
use Test;

# A `pull-one` that returns a bound attribute (`my $this := $!value` with
# `$!value := IterationEnd` set earlier) hands back the cell holding the
# sentinel: the Seq consumer must read what it holds and stop, not emit
# IterationEnd values. Found with Sequence::Generator's `UnendingPred`.
# Identical under raku.

plan 2;

class Down does Iterator {
    has $!value;
    method new(\first) { self.bless(:value(first)) }
    submethod BUILD(:$!value) { }
    method pull-one() is raw {
        unless (my $this := $!value) =:= IterationEnd {
            my $next = $this.pred;
            if $next ~~ Failure {
                $next.Bool;
                $!value := IterationEnd;
            }
            else {
                $!value := $next;
            }
        }
        $this
    }
    method is-lazy(--> True) { }
}

is Seq.new(Down.new("c")).head(8).join(","), "c,b,a", 'ends at the failing .pred';
is Seq.new(Down.new("a")).head(5).join(","), "a", 'a single element';
