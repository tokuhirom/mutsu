use v6;
use lib 't/lib';
use Test;
use TransitiveLeakOuter;
use TransitiveLeakDirect;
use TransitiveLeakGrand;

plan 9;

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
