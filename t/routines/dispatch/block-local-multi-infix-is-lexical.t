use MONKEY-SEE-NO-EVAL;
use Test;

# A block's own `multi infix:<op>` of a built-in operator adds to the family
# in scope around the block, for every use written inside it; a routine
# written outside the block keeps the family it sees itself, even when the
# block calls it (#18617).

plan 18;

class Box {}
class Pt { has $.x }
multi infix:<+>(Pt $a, Pt $b) { Pt.new(x => $a.x + $b.x) }
sub add-outside { (try Box + 1) // 'died' }
sub amp-outside { (try &infix:<+>(Box, 1)) // 'died' }

{
    multi infix:<+>(Box $l, Int $r) { "local" }
    multi infix:<+>(Box $l, Str $r) { "local-str" }
    is Box + 1, 'local', 'the block multi answers in the block';
    is Box + 'a', 'local-str', 'so does its sibling candidate';
    is (Pt.new(x => 1) + Pt.new(x => 2)).x, 3, 'the unit multi still answers in the block';
    is 1 + 2, 3, 'the built-in answers what no candidate takes';
    isnt add-outside(), 'local', 'a routine written outside the block does not see it';
    isnt amp-outside(), 'local', '... nor by its &infix:<+> name';
    is ([+] Box, 1), 'local', 'a reduction in the block uses it';
    is &infix:<+>(Box, 1), 'local', '&infix:<+> in the block is the block family';
    is (1,).map({ Box + 1 }).List, ('local',), 'a map callback in the block uses it';
    is EVAL('Box + 1'), 'local', 'an EVAL in the block sees it';
    is "{Box + 1}", 'local', 'an interpolated block in the block uses it';
    {
        multi infix:<+>(Box $l, Rat $r) { "inner" }
        is Box + 1.5, 'inner', 'a nested block adds a candidate of its own';
        is Box + 1, 'local', '... to the enclosing block family';
    }
}

{
    class Wrap { has $.v }
    multi infix:<==>(Wrap $l, Wrap $r) { $l.v == $r.v }
    ok Wrap.new(v => 1) == Wrap.new(v => 1),
        'a candidate whose body uses the operator on Ints reaches the built-in';
}

my &kept = do {
    multi infix:<*>(Box $l, Int $r) { "kept" }
    -> { Box * 2 }
};
is kept(), 'kept', 'a closure taken out of the block keeps the candidate';
is (try Box * 2) // 'died', 0, 'after the block the built-in answers';

my @seen;
for 1, 2 -> $n {
    multi infix:<->(Box $l, Int $r) { "n=$n" }
    @seen.push: Box - 0;
}
is @seen, <n=1 n=2>, 'each run of a loop body binds its own candidate';

{
    my class A { method Str { self } }
    multi infix:<~>(Str() $a, A $b) { "B" }
    my $a = A.new;
    is "A: $a", 'B', 'an interpolation folds through the block candidate';
}
