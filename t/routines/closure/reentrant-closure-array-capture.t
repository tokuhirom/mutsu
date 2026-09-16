use Test;

plan 1;

# Issue #8540: a `my @array` declared inside a closure body, mutated from
# inside a nested (e.g. `.map`) callback, lost the mutation when the
# enclosing closure was reentered (recursion through the same compiled
# body) while that callback was still running.
#
# `make(&next)` returns a fresh `-> @xs {...}` closure each call; both the
# outer and the leaf closure below share the SAME compiled body. The outer
# closure's `.map` callback calls `&next` (the leaf, running the same
# template reentrant on the call stack) BEFORE it appends into its own
# `@flat` — exactly the ordering that used to let the leaf's own `my
# @flat;` declaration clobber the outer's captured binding.
sub make(&next) {
    -> @xs {
        my @flat;
        @xs.map(-> $x {
            my @got = &next(($x,));
            @flat.append($x, |@got);
        });
        @flat;
    }
}

my $leaf = make(-> @xs { Empty });
my $outer = make($leaf);

is $outer((1,)).List, (1, 1).List,
    'a reentrant closure call does not clobber the outer captured array (#8540)';
