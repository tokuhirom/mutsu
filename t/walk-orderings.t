use Test;

plan 11;

# Mu.WALK walks the class hierarchy in a requested order and returns a WalkList
# that is both list-iterable (the per-level candidate closures) and invocable
# (calls each candidate on the original invocant). Mirrors the orderings in
# roast S12-introspection/walk.t.
class A { method m {'A'}; submethod sm {'a'} }
class B { method m {'B'}; submethod sm {'b'} }
class C is A is B { method m {'C'}; submethod sm {'c'} }
class D is A { method m {'D'}; submethod sm {'d'} }
class E is C is D { method m {'E'}; submethod sm {'e'} }

sub cand_order(@cands, $instance) {
    my $r = '';
    $r ~= $_($instance) for @cands;
    $r;
}

my $x = E.new;

my @cands;
@cands = $x.WALK(:name<m>, :canonical);  is cand_order(@cands, $x), 'ECDAB', ':canonical (C3 MRO)';
@cands = $x.WALK(:name<m>);              is cand_order(@cands, $x), 'ECDAB', ':canonical is the default';
@cands = $x.WALK(:name<m>, :super);      is cand_order(@cands, $x), 'CD',    ':super (direct parents)';
@cands = $x.WALK(:name<m>, :breadth);    is cand_order(@cands, $x), 'ECDAB', ':breadth (BFS)';
@cands = $x.WALK(:name<m>, :descendant); is cand_order(@cands, $x), 'ABCDE', ':descendant';
@cands = $x.WALK(:name<m>, :ascendant);  is cand_order(@cands, $x), 'ECABD', ':ascendant';
@cands = $x.WALK(:name<m>, :preorder);   is cand_order(@cands, $x), 'ECABD', ':preorder is :ascendant';

# Submethods are walked per level too (not inherited via normal dispatch).
@cands = $x.WALK(:name<sm>);             is cand_order(@cands, $x), 'ecdab', ':canonical works with submethods';

# Invoking the WalkList calls each method on the original invocant.
is $x.WALK(:name<m>)().join, 'ECDAB', 'invoking the WalkList runs each method';

# A method found on only one level yields a single candidate.
class N is A { method only-here {'N!'} }
my $n = N.new;
is $n.WALK(:name<only-here>).elems, 1, 'a single-level method yields one candidate';
is $n.WALK(:name<only-here>)().join, 'N!', 'and invokes to that one result';
