# A wrapped method called from inside another method's wrapper loses its own wrap chain

Found by the ADR-0019 E9b design pass (2026-08-13, Rakudo v2026.06). The method-wrap entry
sites guard on the GLOBAL `is_inside_wrap_dispatch()` (`class_dispatch.rs:341`,
`vm_call_method_compiled.rs:282-284`), which suppresses every wrap chain while ANY wrap
dispatch is live — including a completely unrelated method's.

## Divergence

```raku
class A { method x() { "x-orig" } }
class B { method y() { "y-orig" } }
A.^lookup('x').wrap(-> $self { "x-wrap[" ~ callsame() ~ "]+" ~ B.new.y });
B.^lookup('y').wrap(-> $self { "y-wrap[" ~ callsame() ~ "]" });
say A.new.x;
# raku:  x-wrap[x-orig]+y-wrap[y-orig]
# mutsu: x-wrap[x-orig]+y-orig          (B's chain silently skipped)
```

## Root cause

The guard exists only to stop the method-wrap "synthetic original" — a fabricated sub tagged
`__mutsu_method_wrap_original` whose advance leg re-enters `call_method_with_values` by name
(`builtins_dispatch_next.rs:425-433`) — from re-entering its own chain. But
`is_inside_wrap_dispatch()` is `!wrap_dispatch_stack.is_empty()` (`accessors_state.rs:875-877`),
which is true for the whole dynamic extent of every wrapper body, so any method call made FROM
a wrapper has its chain suppressed.

## Fix

Structural, via ADR-0019 E9b-2 (see the "E9b design" section of
`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`): when method wraps become
deferral-frame prefix entries, the original method is invoked directly as a resolved
candidate — there is no by-name re-entry left to protect, so the global guard is deleted and
a nested call to a different wrapped method enters its own chain like any fresh dispatch.
The raku-valued pin for this probe lands with that slice. A narrower interim fix (guard on
"this exact (class, method, candidate) is already being wrap-dispatched" instead of the
global stack-empty check) is possible but churns code E9b-2 deletes.
