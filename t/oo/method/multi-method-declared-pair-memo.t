use v6;
use Test;

plan 12;

# A multi method with many candidates ranks its declared types pairwise for
# every dispatch with a new argument-type key. The pairwise answers depend on
# the declared types only, so repeated dispatches must keep agreeing.
class Node { }
class Lit is Node { }
class IntLit is Lit { }
class StrLit is Lit { }
class Op is Node { }
class Infix is Op { }
class Prefix is Op { }

class Walker {
    multi method w(Node $n)    { 'Node' }
    multi method w(Lit $n)     { 'Lit' }
    multi method w(IntLit $n)  { 'IntLit' }
    multi method w(StrLit $n)  { 'StrLit' }
    multi method w(Op $n)      { 'Op' }
    multi method w(Infix $n)   { 'Infix' }
    multi method w(Prefix $n)  { 'Prefix' }
    multi method w(Str $s)     { 'Str' }
    multi method w(Int $i)     { 'Int' }
    multi method w(Any $a)     { 'Any' }
    multi method w(Num $n, Num $m) { 'Num2' }
    multi method w(Int $n, Int $m) { 'Int2' }
    multi method w(Numeric $n, Numeric $m) { 'Numeric2' }
    multi method w(Mu $m)      { 'Mu' }
    multi method w(Cool $c)    { 'Cool' }
    multi method w(Rat $c)     { 'Rat' }
    multi method w(Positional $c) { 'Positional' }
    multi method w(Associative $c) { 'Associative' }
}

my $w = Walker.new;
for 1..3 -> $round {
    is $w.w(IntLit.new), 'IntLit', "round $round: the narrowest class wins";
    is $w.w(Infix.new), 'Infix', "round $round: a sibling hierarchy";
    is $w.w(Node.new), 'Node', "round $round: the root";
    is $w.w(1.5), 'Rat', "round $round: a core type";
}
