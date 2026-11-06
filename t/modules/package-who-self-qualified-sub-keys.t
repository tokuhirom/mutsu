use Test;

# A sub named with its enclosing package's own name (`sub M::e2`) is a plain
# lexical that is only exported (#16819); `our sub M::o2` is the stash member
# whose name is the literal `M::o2`.
plan 5;

module A {
    sub A::e2 is export { 1 }
    our sub A::o2 { 2 }
}
is A.WHO.keys.sort.join(' '), '&A::o2 EXPORT', 'only the our sub is a member of A';
is A::EXPORT::ALL.WHO.keys.sort.join(' '), '&A::e2', 'the exported sub is in EXPORT::ALL';

module B { our sub B::e3 is export { 3 } }
is B.WHO.keys.sort.join(' '), '&B::e3 EXPORT', 'an exported our sub is listed once, qualified';

module C { sub e4 is export { 4 } }
is C.WHO.keys.sort.join(' '), 'EXPORT', 'a plain exported lexical sub is not a member';

module D { sub D::l {5} }
is D.WHO.keys.elems, 0, 'a my-scoped self-qualified sub is not a member';

