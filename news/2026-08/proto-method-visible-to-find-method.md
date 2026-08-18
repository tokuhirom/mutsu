# A zero-candidate `proto method` is now visible to `.^find_method`/`.^lookup`/`.can`

```raku
class Foo { proto method bar {*} }
say Foo.^find_method('bar').defined;   # raku: True   mutsu (before): False
say Foo.can('bar');                    # raku: (bar)  mutsu (before): ()
```

Found while investigating
`todo/tickets/class-redeclaration-rollback-loses-proto-method.md`'s own
"repro sketch" (explicitly marked "not yet verified/reduced" there). Building
a real repro showed the sketch's premise didn't hold — the inner class body
never actually failed/rolled back in mutsu the way it does in real Rakudo
(an undeclared bareword call inside a method body is a compile-time
`X::Undeclared` in Rakudo but not yet in mutsu, a separate, unrelated gap —
see `t/eval-core-term-constant-call-undeclared-class.t` for the closest
existing coverage of that class of check). Stripping the repro down further
(dropping the `EVAL`/redeclaration machinery entirely) isolated a much more
fundamental and reproducible bug: a bare `proto method bar {*}`, with **no**
regular candidates declared alongside it, was invisible to introspection
from the moment it's first declared — nothing about rollback or
redeclaration was required to trigger it.

## Root cause

`classhow_lookup_impl` (`src/runtime/methods_classhow_lookup.rs`), the shared
implementation behind `.^lookup`/`.^find_method`/`.can`, only ever consulted
`Registry::user_method_overloads` (the per-level list of real method
candidates) when walking a receiver's MRO. `MethodEntry::proto` (the
`proto method`/`proto submethod` column added in ADR-0019 E8/E8b/E8c) lives
in a separate registry column entirely, read by `Registry::method_entry_proto`
— and nothing in `classhow_lookup_impl` ever consulted it. A proto with zero
candidates has no `user_method_overloads` row at all, so every MRO level for
it was skipped, and the whole lookup fell through to `None`.

Actual method *dispatch* was unaffected: `run_proto_method`
(`src/runtime/dispatch_proto.rs`) already builds a synthetic `MethodDef` from
the proto's `FunctionDef` to run it, so calling a proto with real candidates
always worked. Only *introspection* on a bare, candidate-less proto had no
equivalent path.

## Fix

`classhow_lookup_impl` now falls back to `method_entry_proto` at each MRO
level when there is no usable candidate row, building the SAME synthetic
`MethodDef` shape `run_proto_method` already constructs for dispatch — so
introspection and dispatch agree on what a proto "is" as a method.

Regression tests: `t/proto-method-visible-to-find-method.t`.

## What this means for the original ticket

`todo/tickets/class-redeclaration-rollback-loses-proto-method.md`'s specific
claim — that a FAILED class redeclaration's rollback selectively fails to
restore a proto row while correctly restoring everything else — remains
unverified. Constructing the exact trigger it describes (`compose_class_
parent_roles` failing during a genuine redeclaration) turned out to need its
own investigation, since mutsu's redeclaration error semantics (`X::Redeclaration`)
diverge from Rakudo's in ways that made a same-shape repro non-trivial in the
time available. Retired as resolved-by-different-fix; if the original rollback
scenario is independently confirmed later, it should be filed fresh with a
verified repro.
