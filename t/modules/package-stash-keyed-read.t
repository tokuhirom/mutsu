use Test;

# `Pkg::<$x>` / `Pkg::{$k}` read one key without building the whole stash
# (#9171). Every form must agree with the whole-stash read (`Pkg::.AT-KEY`,
# `Pkg::.keys`), which is built by the same membership rules.

plan 22;

package P {
    our $x = 1;
    our @a = 1, 2, 3;
    our %h = k => 'v';
    our sub f() { 'f' }
    my $lexical = 'hidden';
    our proto sub pr(|) {*}
    our multi sub pr(Int) { 'int' }
}

is P::<$x>, 1, 'scalar';
is P::<@a>.elems, 3, 'array';
is P::<%h><k>, 'v', 'hash';
is P::<&f>(), 'f', 'our sub';
is P::<&pr>(1), 'int', 'our proto';
nok P::<$lexical>.defined, 'a my-scoped variable is not a member';
nok P::<$nope>.defined, 'a missing member reads undefined';
nok P::<$nope>:exists, 'a missing member does not exist';
ok P::<$x>:exists, 'a present member exists';

my $k = '$x';
is P::{$k}, 1, 'computed key';
is P::{$k}, P::.AT-KEY('$x'), 'keyed read agrees with the whole stash';

# A later write is seen by the next read.
$P::x = 42;
is P::<$x>, 42, 'a write through the qualified name is visible';

# Nested packages are reached by their short name too (the stash suffix rule).
module Outer {
    package Inner {
        our $deep = 'deep';
    }
    our sub get() { Inner::<$deep> }
}
is Outer::Inner::<$deep>, 'deep', 'nested package by full name';
is Outer::get(), 'deep', 'nested package by short name from inside';

# An `our` declared in a branch that never ran is still a member.
package Q {
    if False { our $never = 1 }
}
ok Q::<$never>:exists, 'dead-branch our is a member';
nok Q::<$never>.defined, 'dead-branch our is undefined';

# The key may name any sigil form of the same bare name.
package S {
    our $n = 's';
    our @n = 'a';
}
is (S::<$n>, S::<@n>[0]).join(','), 's,a', 'same bare name, different sigils';

# Non-sigiled keys (sub-packages, classes) keep working.
package R {
    class C { }
}
ok R::<C> === R::C, 'class member';
ok Outer::<Inner>:exists, 'sub-package member';

# Pseudo-packages keep their own semantics.
is GLOBAL::<P>.^name, 'P', 'GLOBAL:: keyed read';
my $lex = 'lex';
is MY::<$lex>, 'lex', 'MY:: keyed read';
{
    my $*dyn = 'dyn';
    is DYNAMIC::<$*dyn>, 'dyn', 'DYNAMIC:: keyed read';
}
