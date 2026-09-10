use v6;
use lib 't/lib';
use MONKEY-SEE-NO-EVAL;
use Test;
use TransitiveLeakOuter;
use TransitiveLeakDirect;
use TransitiveLeakGrand;
use TransitiveLeakConstA;
use TransitiveLeakConstB;

plan 32;

# A module body runs in the *caller's* env, so the short-name type aliases a
# module's own `use` statements install used to be left behind in whatever
# scope triggered the load. That made a transitively-`use`d module's classes
# resolvable bare from a file that never `use`d it, where rakudo reports an
# "Undeclared name" (issue #7555, item 1 divergence (b)):
#
#     # TransitiveLeakOuter.rakumod:  use TransitiveLeakInner; unit module ...
#     use TransitiveLeakOuter;
#     InnerClass.new;    # rakudo: Undeclared name
#
# The aliases are recorded against the declaring module instead, so its own
# routines keep resolving them.

# What the importer legitimately gets from the module it actually used.
is OuterClass.new.who, 'outer-class', 'the used module exports its own class';

# What it must NOT get: anything the used module reached through its own `use`.
ok ::('InnerClass') ~~ Failure,
    'a transitively-used module class does not leak into the importer';
ok ::('InnerRole') ~~ Failure, 'a transitively-used role does not leak either';
# The module's own name is not visible either -- the importer never asked
# for the package, only for what its own module exported.
ok ::('TransitiveLeakInner') ~~ Failure,
    'the transitively-used module package itself does not leak';

# The declaring module's own routines and methods must still see the short
# names it imported -- that is the whole point of recording them against the
# module rather than leaving them in the loading frame.
is outer-uses-inner(), 'inner-class/inner-const',
    'the module sub still resolves its own imported class and constant';
is OuterHolder.new.make-inner, 'inner-class',
    'a method of a class the module declares still resolves them too';

# The sibling-package NativeCall shape (`unit module Foo::Native; class Handle`
# used bare from a sibling class's method) keeps working: a module that `use`s
# another one references its classes bare from its own method bodies.
is direct-probe(), 'TransitiveLeakInner::InnerClass',
    'a module referencing another module class bare still resolves it';

# Two hops out: a module that `use`s TransitiveLeakOuter must not see what
# *Outer* imported either, including from a method body -- which resolves bare
# names through its own class's package chain, a separate path from `env`.
isnt grand-probe(), 'TransitiveLeakInner::InnerClass',
    'a two-hop transitive class is not resolvable from the middle importer';
is grand-outer-probe(), 'TransitiveLeakOuter::OuterClass',
    'while what that importer did import stays resolvable';

# ---------------------------------------------------------------------------
# #7787: the same contract for a `unit module`'s own file-scope `constant`s and
# enum values. They are package symbols of the declaring compunit in rakudo, so
# the bare name must not be resolvable in whatever scope triggered the load --
# neither for the module the importer actually used, nor for one it only
# reached transitively.

sub missing($name) {
    my $v = ::($name);
    ($v.defined and $v !~~ Failure) ?? $v.gist !! 'MISSING';
}

is missing('OUTER-PRIVATE'), 'MISSING',
    'a used module constant does not leak into the importer';
is missing('OUTER-GAMMA'), 'MISSING',
    'a used module enum value does not leak into the importer';
is missing('INNER-PRIVATE'), 'MISSING',
    'a transitively-used module constant does not leak either';
is missing('INNER-ALPHA'), 'MISSING',
    'nor does a transitively-used module enum value';
is missing('INNER-OUR'), 'MISSING',
    'an `our constant` is no more visible to the importer than a bare one';
is missing('OuterEnum'), 'MISSING', 'nor is the enum type name itself';
is missing('InnerEnum'), 'MISSING', 'nor a transitively-used enum type name';

# What the importer legitimately keeps: an `is export` constant, and package-
# qualified access to the module it actually used.
is outer-probes-inner-const(), 'inner-const',
    'an `is export` constant still reaches the module that used it';
is TransitiveLeakOuter::OUTER-PRIVATE, 'outer-private',
    'the used module constant is still a package symbol of that module';
is TransitiveLeakOuter::OUTER-GAMMA.key, 'OUTER-GAMMA',
    'and so is its enum value';

# The declaring module's own code must still read them -- from a top-level sub
# and from a method of a class it declares, which resolve bare names by
# different paths.
is outer-reads-private(), 'outer-private/OUTER-GAMMA',
    'the module sub still reads its own constant and enum value';
is OuterReader.new.peek(), 'outer-private/OUTER-DELTA',
    'a method of a class the module declares reads them too';
is outer-probes-inner-reads(), 'inner-private/INNER-ALPHA/inner-our',
    'a transitively-loaded module sub still reads its own, including `our constant`';
is outer-probes-inner-method(), 'inner-private/INNER-BETA',
    'and so does a method of a class it declares';

# A module does not see an unexported constant of a module it `use`s.
is outer-peeks-inner-private(), 'MISSING',
    'the middle module does not see the inner module unexported constant';

# The leaked binding was not merely extra, it was arbitrary: whichever module
# loaded first won the name. Both must read their own value, and neither may
# install the short name in the importer.
is a-reads-shared(), 'from-A', 'each module reads its own same-named constant (A)';
is b-reads-shared(), 'from-B', 'each module reads its own same-named constant (B)';
is missing('SHARED-CONST-NAME'), 'MISSING',
    'and no arbitrary winner is left behind in the importer';

# ---------------------------------------------------------------------------
# #7797: the same leak, reached through a PACKAGE-QUALIFIED name instead of a
# bare one. Every symbol below is ALSO a package symbol of the module that
# declares it (`TransitiveLeakInner::InnerConst`, `::InnerClass`, ...), and
# that qualified form is reachable from Inner's own code, and from Outer
# (which DOES `use TransitiveLeakInner;`) -- but must NOT leak to a compunit
# that only reaches Inner transitively, through Outer's `use`, exactly as the
# bare names above don't. Unlike `::()`, `EVAL` also covers a method call,
# not just a bare symbol lookup.
sub qmissing($code) {
    my $r = try EVAL $code;
    $! ?? 'MISSING' !! $r.gist;
}

is qmissing('TransitiveLeakInner::InnerConst'), 'MISSING',
    'a transitively-used module exported constant is not reachable qualified either';
is qmissing('TransitiveLeakInner::INNER-PRIVATE'), 'MISSING',
    'nor is its unexported constant, qualified';
is qmissing('TransitiveLeakInner::InnerEnum'), 'MISSING',
    'nor its enum type, qualified';
is qmissing('TransitiveLeakInner::InnerClass.new.who'), 'MISSING',
    'nor its class, qualified';

# What stays reachable qualified: a module the importer DID `use` directly
# (TransitiveLeakOuter::OUTER-PRIVATE above already pins the constant case).
is TransitiveLeakOuter::OuterClass.new.who, 'outer-class',
    'the used module class is still reachable qualified';
