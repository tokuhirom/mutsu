use Test;

plan 18;

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

# :omit / :include filter candidates by calling the callback with the OWNER
# type object.
class P is A { method m {'P'}; method only {'O'} }     # P can .only; A cannot
my $p = P.new;
@cands = $p.WALK(:name<m>, :omit({ .^can('only') }));
is cand_order(@cands, $p), 'A', ':omit drops levels for which the callback is truthy';
@cands = $p.WALK(:name<m>, :include({ $^c.gist ~~ /P/ }));
is cand_order(@cands, $p), 'P', ':include keeps only levels for which the callback is truthy';

# WalkList batch-invoke / reverse / quiet methods.
is-deeply $x.WALK(:name<m>).invoke.List, <E C D A B>, '.invoke returns the result list';
is-deeply $x.WALK(:name<m>).reverse.invoke.List, <B A D C E>, '.reverse flips the order';
isa-ok $x.WALK(:name<m>), WalkList, 'WALK returns a WalkList';

# A WalkList candidate reports the walked method name via .name.
my ($cand) = $x.WALK(:name<m>);
is $cand.name, 'm', 'a candidate .name is the method name';

# WALK over a built-in type (Grammar) finds its native methods.
my ($g) = Grammar.WALK(:name<parse>);
is $g.name, 'parse', 'Grammar.WALK finds the native parse method';
