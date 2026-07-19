use v6;
use Test;

# Set-operator compound assignment (`∪=`, `∩=`, `(-)=`, `(^)=`, `(+)=`, `(.)=`
# and their Unicode/ASCII variants) on an *indexed* lvalue — a hash element,
# array element, or attribute subscript. Regression: mutsu handled the scalar
# form (`$x ∪= $s`) but the infix set-op parser consumed `∪` in `%h<k> ∪= $s`
# and choked on the trailing `=`, so `%h<k> ∪= …` died with "expected
# expression after set operator". (Hit by Trie's `%!decendents{char} ∪= …`.)
#
# Sets/Bags are unordered, so compare a canonical sorted view, not `.gist`.

plan 9;

sub set-canon($s)  { $s.keys.sort.join(',') }
sub bag-canon($b)  { $b.pairs.map({ "{.key}:{.value}" }).sort.join(',') }

# Hash element, Unicode union.
{
    my %h; %h<a> = (1, 2).Set;
    %h<a> ∪= (3,).Set;
    is set-canon(%h<a>), '1,2,3', 'hash element ∪= (union)';
}

# Hash element, ASCII union.
{
    my %h; %h<a> = (1, 2).Set;
    %h<a> (|)= (3,).Set;
    is set-canon(%h<a>), '1,2,3', 'hash element (|)= (ASCII union)';
}

# Array element, intersection.
{
    my @a; @a[0] = (1, 2, 3).Set;
    @a[0] (&)= (2, 3, 4).Set;
    is set-canon(@a[0]), '2,3', 'array element (&)= (intersection)';
}

# Hash element, set difference (Unicode).
{
    my %h; %h<a> = (1, 2, 3).Set;
    %h<a> ∖= (2,).Set;
    is set-canon(%h<a>), '1,3', 'hash element ∖= (set difference)';
}

# Hash element, symmetric difference (ASCII).
{
    my %h; %h<a> = (1, 2, 3).Set;
    %h<a> (^)= (3, 4).Set;
    is set-canon(%h<a>), '1,2,4', 'hash element (^)= (symmetric difference)';
}

# Bag element, baggy addition.
{
    my %h; %h<a> = (1, 1, 2).Bag;
    %h<a> (+)= (2, 3).Bag;
    is bag-canon(%h<a>), '1:2,2:2,3:1', 'bag element (+)= (baggy addition)';
}

# Attribute hash element inside a method (method-call lvalue path).
{
    class C {
        has %.h;
        method add-to($k, $s) { %!h{$k} ∪= $s }
    }
    my $c = C.new;
    $c.h<x> = (1,).Set;
    $c.add-to('x', (5, 6).Set);
    is set-canon($c.h<x>), '1,5,6', 'attribute hash element ∪= inside a method';
}

# The scalar form still works (no regression).
{
    my $x = (1, 2).Set;
    $x ∪= (5,).Set;
    is set-canon($x), '1,2,5', 'scalar ∪= still works';
}

# Plain infix ∪ (not a compound assign) still works (no regression).
{
    is set-canon((1, 2).Set ∪ (3,).Set), '1,2,3', 'plain infix ∪ still works';
}
