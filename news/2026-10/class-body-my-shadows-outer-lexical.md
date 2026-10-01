# A class-body `my` shadows the declaring scope's same-named lexical

```raku
my $x = 1; class A { my $x = 7; method m { $x } }; say A.m
```

printed `1`; rakudo prints `7`. Worse, a method that wrote to the variable
(`method inc { ++$x }`) clobbered the *outer* `$x` instead of the class's own.

A class-body `my`/`state` is persisted as a class static, and its methods get
that store injected on entry. But a method also captures every lexical of the
declaring frame it reads (`method_outer_lexical_slots`), and that captured
environment is applied *after* the statics are injected, as authoritative. When
the class body re-declares a name the declaring frame also has, the outer
variable's value therefore replaced the static, and writes landed on the outer
slot.

The class declaration plan now drops the names its own body declares from the
outer lexicals it lets methods capture, through one helper shared with the role
plan (a role-body `my` had the identical bug: `role R { my $x = 7; method m {
$x } }` composed under an outer `$x` answered the outer value). The names come
from `declared_static_names`, the same precomputed top-level `my`/`state`
scan registration already uses to decide what a class body persists, so there
is no second notion of "what the body declares". A `my` in a nested block of
the body, a differently-named outer lexical and a method parameter are
unaffected, as in rakudo.

Pinned by `t/oo/class-body-my-shadows-outer-lexical.t` (20 tests, rakudo's
output for every shape: `my`, `state`, `@`/`%`, `my sub`, `our`, writes through
one or two methods, a per-class counter, closures, a submethod, a class
declared in a routine, and the role case).

One shape is deliberately not pinned: a method written *before* the body's
`my` (`class A { method m { $x }; my $x = 7 }`). Rakudo answers an internal
`(LoweredAwayLexical)` marker there, which is a rakudo quirk rather than a
behaviour to copy; mutsu answers the class static.
