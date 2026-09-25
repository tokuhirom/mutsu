use Test;

# A `where` constraint on a single-argument-rule slurpy parameter: sigilless
# `+bar where ...` failed to parse (WhereList's
# `sub foo (+bar where all-items ...)`), and neither `+bar` nor `+@bar`
# enforced the constraint. Like a `*@a` slurpy, it sees the collected list. A
# sigilless parameter is named bare in the binding error, as in rakudo.

plan 8;

sub s(+bar where *.elems > 1) { bar.elems }
is s(1, 2, 3), 3, '+bar where: passing constraint';
throws-like { s(5) }, X::TypeCheck::Binding::Parameter,
    message => /"parameter 'bar'; expected anonymous constraint"/,
    '+bar where: failing constraint dies, parameter named bare';

sub a(+@bar where *.elems > 1) { @bar.elems }
is a(1, 2, 3), 3, '+@bar where: passing constraint';
throws-like { a(5) }, X::TypeCheck::Binding::Parameter,
    message => /"parameter '@bar'"/, '+@bar where: failing constraint dies';

multi m(+a where *.elems == 1) { 'one' }
multi m(+a) { 'many' }
is m(1), 'one', '+a where discriminates multi candidates (one)';
is m(1, 2), 'many', '+a where discriminates multi candidates (many)';

sub t(+bar where { .all ~~ Int }) { bar.elems }
is t(1, 2), 2, '+bar where with a block constraint';

sub x(Any \v where * > 1) { v }
throws-like { x(0) }, X::TypeCheck::Binding::Parameter,
    message => /"parameter 'v'"/, 'sigilless \v is named bare in a binding error';
