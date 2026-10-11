# A role mixed in on another thread is visible everywhere

`$obj does R` reblesses the object in place into the mixin type `C+{R}`, so every alias of
the object sees the new class name at once — on every thread. But each thread has its own
copy-on-write snapshot of the type registry, and only the thread that ran `does` composed
`C+{R}`. On any other thread `$obj ~~ R` answered `False`, a method of `R` threw
`X::Method::NotFound`, and `.^roles` came back empty:

```raku
role Ctx { method hi { 'hi' } }
class Foo {}
my $f = Foo.new;
await start { $f does Ctx };
say $f ~~ Ctx;   # was False, now True
say $f.hi;       # was X::Method::NotFound, now "hi"
```

Mixin types are now published process-wide as recipes — the base class and the roles composed
onto it — and a thread that meets an unknown mixin type name composes the same recipe into its
own registry, base first for a stacked `C+{A}+{B}` (ADR-11456). The probe on the method
resolution and type-matching paths costs one atomic load until a program first mixes a role
in. A role body that runs at composition runs again on the adopting thread, as role-body
lexicals are already per-thread; #12825 tracks giving them one shared home.

Verified on the real LogP6 (whose `get-context` does `$*THREAD does LogP6::ThreadLocal` on
each logging thread): a worker's `Thread` object, read from the main thread, smartmatches
`LogP6::ThreadLocal` and answers its `._context` exactly as under rakudo.
