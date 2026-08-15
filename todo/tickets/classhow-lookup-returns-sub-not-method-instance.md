# `.^lookup`/`.^find_method` return a `Sub`-shaped value, not a `Method` instance, so `Method`-only accessors silently misbehave

Found while scoping ADR-0019 Phase F box F1 item 2
(`todo/deep/adr0019-f1-f2-introspection-canonical-source.md`).

## Repro

```
$ raku -e 'my $m = (42).^lookup("floor"); say $m.is_dispatcher; say $m.multi'
False
0
$ ./target/debug/mutsu -e 'my $m = (42).^lookup("floor"); say $m.is_dispatcher; say $m.multi'
<composed-method:is_dispatcher>
<composed-method:multi>
```

Real Raku prints `False`/`0` (a `Method`'s `is_dispatcher`/`multi` accessors). mutsu prints a bogus
`<composed-method:NAME>` string for *any* unrecognized method call on the value — it never errors
and never returns the right answer.

Also reproduces on a plain user method, not just native ones:

```
$ ./target/debug/mutsu -e 'class A { method foo {} }; say A.^lookup("foo").is_dispatcher'
<composed-method:is_dispatcher>
```

## Root cause

`.^methods`/`.^method_table`/`.can` (`src/runtime/methods_classhow_method_obj.rs`,
`collect_class_methods`/`class_method_table`/`collect_can_methods`) build a `Method`
**`Instance`**-shaped `Value` via `make_method_object_with_owner`, carrying `is_dispatcher`,
`signature`, `candidates`, etc. as real instance attributes.

`.^lookup`/`.^find_method` (`src/runtime/methods_classhow_lookup.rs`, `classhow_lookup`/
`classhow_lookup_impl`) instead build a **`Sub`**-shaped `Value::make_sub` — a callable, not an
`Instance` with `Method`'s attributes.

When an unrecognized method (`is_dispatcher`, `multi`, ...) is called on a `Sub` value, dispatch
falls into the "method calls on callables compose" fallback in
`src/runtime/methods_instance_ops.rs` (~line 2117): "calling `.foo` on a `Sub` means apply `foo` to
the Sub's *return value*". That fallback builds a new composed-callable `Sub` named
`<composed-method:foo>` and returns it — since nothing ever calls it, printing it just shows the
placeholder name, and no error is ever raised.

## Why this is a real (not cosmetic) gap

Any `Method`-only introspection accessor (`is_dispatcher`, `multi`, `candidates` in some cases,
`package`/`name` happen to already work because those are special-cased elsewhere) is unreachable
on the result of `.^lookup`/`.^find_method`, silently returning garbage instead of erroring or
answering correctly. This is exactly the kind of surface ADR-0019 Phase F (F1/F2, "derive
`.^methods`/`.^can`/method MRO views from the resolver/table" — same unification PLAN.md §5 calls
for) is meant to fix, but it is a distinct, smaller bug from F1's native-metadata gap: it's a
representation mismatch (two different "this is a method" `Value` shapes that don't interoperate),
not a missing-data problem.

## Why this is deep, not a quick ticket

Unifying the two representations (making `.^lookup` return the same `Method`-`Instance` shape
`.^methods` does, or vice versa) touches:

- `.wrap` on a `.^lookup` result, which today relies on the `Sub` shape's env-carried
  `__mutsu_lookup_class`/`__mutsu_lookup_method` tags (see `make_method_object_with_owner`'s doc
  comment) to register a wrap chain — a `Method`-`Instance`-shaped lookup result would need the
  same wrap-registration path wired differently (the `Instance` already carries these same tags for
  `.wrap` from `.^methods(:local)`, so this may already be closer to solved than it looks, but needs
  verification).
- Direct callability: `.^lookup("foo")(invocant, args)` presumably still needs to work, which is
  why `.^lookup` returns something callable today — a `Method`-`Instance` is not directly callable
  without dispatch support for calling an `Instance`.
- All existing callers of `classhow_lookup`/`classhow_find_method` that assume a `Sub`-shaped
  result (arity/param inspection, `.wrap`, `.^can`'s reuse in some paths) need an audit.

Best done as part of the F1/F2 design once the native-metadata ground-truth pass
(`todo/deep/adr0019-f1-f2-introspection-canonical-source.md`) lands, since both are about making
introspection surfaces agree with each other and with the canonical dispatch table.

## Progress (2026-08-15): the direct-caller audit is smaller than it looked

After landing F1's mechanism slice (`.package` and `.signature` defaults for native `Method`
Instances -- see the linked design doc), did a quick read-only grep-audit of the "all existing
callers... need an audit" bullet above, since it was the vaguest of the three blockers. Only 5 real
call sites in the whole codebase touch `classhow_lookup`/`classhow_find_method`:
`methods_classhow_dispatch.rs` (2, the `.^lookup`/`.^find_method` opcode handlers themselves, which
return the result straight to user code -- these are exactly the sites that need the result to be
BOTH callable and Method-accessor-bearing, i.e. exactly where the representation choice matters
most), `builtins_dispatch_next.rs` (1, `nextsame`-family redispatch), `methods_instance_ops.rs` (1,
inside the general method-call fallback), and `methods_classhow_method_obj.rs` (1, a `.is_some()`
existence check only -- does not touch the returned value's shape at all, so free of this concern).
Not a large fan-out; the "audit" bullet is not the blocker the other two are.

**The remaining two blockers (`.wrap`'s tag reuse and direct callability) are still the real
open questions**, and still need real design/verification, not a grep. In particular: does making
`.^lookup`'s result a `Method` Instance require teaching mutsu's call dispatch to invoke an
`Instance` value directly (a new, general capability), or is there a narrower fix that keeps the
returned value Sub-shaped for *calling* purposes while making every `Method`-only accessor
(`is_dispatcher`, `multi`, `.candidates`, `.signature`, `.package`, ...) answer correctly on it --
extending #6420's tag-based approach comprehensively instead of swapping representations? The
narrower fix keeps the well-known #6420 pattern (env tags read by the general dispatch fallback) but
risks becoming an ever-growing patch list, one accessor at a time, rather than closing the gap once.
This design choice is the actual next step before any implementation.

## Progress (2026-08-14, #6420)

The `.is_dispatcher`/`.multi` symptom is fixed with a scoped patch, not the representation
unification this file describes. `methods_instance_ops.rs`'s general method-dispatch fallback now
answers both accessors directly on the `Sub`-shaped value (keyed off the existing
`__mutsu_lookup_*`/`__mutsu_callable_type` env tags, plus a new `__mutsu_is_multi_candidate` tag
set only on `.candidates[N]` entries), matching `raku` ground truth: a non-multi method or
submethod answers both `False`; a multi method's dispatcher-shaped value (what `.^lookup`/
`.^find_method` return for the whole family) answers `is_dispatcher` `True` but `multi` falsy;
each individual `.candidates[N]` entry answers `is_dispatcher` `False` but `multi` `True`. Pin:
`t/classhow-lookup-method-is-dispatcher-multi.t` (byte-for-byte identical TAP output against
`raku`).

Note the exact repro at the top of this file (`(42).^lookup("floor")`, the *native*-method case)
no longer reproduces the `<composed-method:NAME>` bug on current `main` — it now raises a clean
"No such method 'is_dispatcher' for invocant of type 'Method'" error instead (unchanged by this
patch; some other change fixed that path independently before this session). The user-defined-class
case shown further down (`class A { method foo {} }; A.^lookup("foo").is_dispatcher`) DID still
reproduce the bogus-callable bug and is what this patch fixes.

The representation mismatch itself (Sub vs. Method Instance) — and everything in "Why this is deep,
not a quick ticket" above — remains open.

## Progress (2026-08-14): the scoped patch's coverage is narrower than "any Method value"

While gathering more F1 raku ground truth (`todo/deep/adr0019-f1-f2-introspection-canonical-source.md`),
found `#6420`'s fix does not cover every case its own description implies. `Int.^lookup("Numeric")`
— a real *multi* native method — still raises `No such method 'is_dispatcher' for invocant of type
'Method'` on current `main`, instead of raku's `True` (raku: `Int.^lookup("Numeric").is_dispatcher`
→ `True`, since `Numeric` is multi with 3 candidates). #6420's fix keys off env tags
(`__mutsu_lookup_*`/`__mutsu_is_multi_candidate`) set only at the call sites its own pin exercised;
a native multi method's `.^lookup` result never gets those tags set, so it falls through to the
still-open "no such method" error rather than the bogus `<composed-method:NAME>` the original repro
showed. Same root cause, different symptom, not yet pinned. Confirms this is best fixed by the
representation unification, not another tag-based patch.

## Progress (2026-08-14, continued): native `is_dispatcher=True` is the COMMON case, not a rare edge

Attempted a narrow, non-representation-unifying fix: extend the `is_dispatcher`/`multi` match arms
in `methods_instance_ops.rs` to also handle `ValueView::Routine` (the shape `.^lookup` returns for a
*native* method, distinct from the `ValueView::Sub` shape `#6420`'s tags live on), defaulting to
`False`/`False` since mutsu has no data on which native methods are Rakudo-core multis. Before
committing this, checked how often that default would actually be wrong via a wider raku sweep —
and it is wrong far more often than expected:

```
$ raku -e 'for <floor ceiling round trim substr Str Int> -> $m { say "$m (Int): " ~ 42.^lookup($m).is_dispatcher }'
floor (Int): False
ceiling (Int): False
round (Int): True
trim (Int): True
substr (Int): True
Str (Int): True
Int (Int): False
$ raku -e 'for <chars flip uc lc trim substr Str Int> -> $m { say "$m (Str): " ~ "abc".^lookup($m).is_dispatcher }'
chars (Str): True
flip (Str): True
uc (Str): True
lc (Str): True
trim (Str): True
substr (Str): True
Str (Str): True
Int (Str): False
```

**`is_dispatcher = True` is the majority case for Cool/Any-declared native methods in real Rakudo**
(most core methods are declared `multi method` for invocant-type-flexibility reasons), not the rare
exception this ticket's earlier entries assumed. A blanket `False` default would therefore be wrong
*more often than right* for this accessor — silently wrong, not just incomplete. Per this project's
"no stubs/hardcoded outputs to fake a result" convention, that is worse than the current clean error,
not better, so **the narrow fix was reverted, not committed**. This sharpens (does not resolve) the
F1 fidelity-slice scope: `is_dispatcher`/`multi`/`.candidates` for native methods is not a rarely-hit
corner needing a handful of hand overrides — it needs real per-native-method multiplicity data across
most of the catalog, on the same order of effort as the `.signature`/`.package` fidelity work already
scoped. See `todo/deep/adr0019-f1-f2-introspection-canonical-source.md`.
