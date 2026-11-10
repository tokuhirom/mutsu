use Test;

# A `multi` that redeclares a core membership or subset operator extends it and
# keeps its (chaining) precedence: `$a ∪ $b ∈ $c` is `($a ∪ $b) ∈ $c`
# (Qwiratry's t/operator/precedence.rakutest, #13370).

plan 4;

class OB { has $.n }
multi sub infix:<∪>(OB $l, OB $r) { OB.new(n => "({$l.n}∪{$r.n})") }
my @seen;
multi sub infix:<∈>(OB $e, Positional $c) { @seen.push($e.n); False }
multi sub infix:<⊆>(OB $e, Positional $c) { @seen.push("sub " ~ $e.n); True }

my $a = OB.new(n => 'a');
my $b = OB.new(n => 'b');
my $q = $a ∪ $b;

my $chained = $a ∪ $b ∈ [$q];
ok $chained ~~ Bool, 'the union is the left operand of the redeclared ∈';
is @seen.tail, '(a∪b)', 'the member is the completed union';

my $sub = $a ∪ $b ⊆ [$q];
ok $sub, 'the same for a redeclared ⊆';
is @seen.tail, 'sub (a∪b)', 'the subset test sees the completed union';
