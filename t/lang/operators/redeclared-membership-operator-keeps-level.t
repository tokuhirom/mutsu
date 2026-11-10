use Test;

# A `multi` that redeclares a core membership operator extends it and
# keeps its (chaining) precedence: `$a ∪ $b ∈ $c` is `($a ∪ $b) ∈ $c`
# (Qwiratry's t/operator/precedence.rakutest, #13370).

plan 2;

class OB { has $.n }
multi sub infix:<∪>(OB $l, OB $r) { OB.new(n => "({$l.n}∪{$r.n})") }
my @seen;
multi sub infix:<∈>(OB $e, Positional $c) { @seen.push($e.n); False }

my $a = OB.new(n => 'a');
my $b = OB.new(n => 'b');
my $q = $a ∪ $b;

my $chained = $a ∪ $b ∈ [$q];
ok $chained ~~ Bool, 'the union is the left operand of the redeclared ∈';
is @seen.tail, '(a∪b)', 'the member is the completed union';
