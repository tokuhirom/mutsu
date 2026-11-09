use Test;

plan 8;

# rakudo: (&) and ∩ are junctive-and (tighter), (|) ∪ (+) (-) (^) junctive-or.
is-deeply (4,5) ∪ (2,3) ∩ (3,4), set(3,4,5), '∪ with ∩ on its right';
is-deeply (2,3) ∩ (3,4) ∪ (4,5), set(3,4,5), '∩ first, then ∪';
is-deeply (1,2) (-) (2,3) ∩ (2,9), set(1), '(-) with ∩ on its right';
is-deeply (1,2) (|) (2,3) (&) (3,4), set(1,2,3), '(|) with (&) on its right';
is-deeply (2,3) (&) (3,4) (|) (4,5), set(3,4,5), '(&) first';

# Redeclaring a core set operator as a multi keeps its precedence: it stays
# looser than the additive operators.
multi sub infix:<∩>(Int $a, Int $b) { $a * $b }
is 2 ∩ 3 + 1, 8, 'a redeclared ∩ stays looser than +';
is 1 + 2 ∩ 3, 9, 'and on the other side';
is (2 ∩ 3) + 1, 7, 'parentheses regroup';
