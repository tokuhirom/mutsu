# ADR-0056: NativeCall's `Pointer`/`CArray`/`long`/... display under `NativeCall::Types::*` — display-only, registry key stays bare

- Status: Accepted (implemented)
- Date: 2026-08-20
- Supersedes: nothing
- Related: `todo/deep/nativecall-types-package-qualification.md` (the investigation this
  ADR closes out), [ADR-0047](0047-type-identity-is-a-declaration-site-not-a-registry-name.md)
  (a different but structurally similar "presentation layer needs one demangling
  helper, the registry key itself must stay untouched" precedent)

## Context

Real Rakudo registers NativeCall's `Pointer`, `CArray`, `void`, `OpaquePointer`, and the
seven C-width integer aliases (`long`, `ulong`, `longlong`, `ulonglong`, `size_t`, `ssize_t`,
`bool`) under the `NativeCall::Types` package. `use NativeCall` imports the short names as
lexical aliases, so `.^name` on the type object reports the fully qualified path:

```
$ raku -e 'use NativeCall; say Pointer.^name; say Pointer[uint8].^name'
NativeCall::Types::Pointer
NativeCall::Types::Pointer[uint8]
```

mutsu reported the bare short name for all ten. Worse, mutsu already had a standing
self-inconsistency: an *instance*'s `.gist` hardcoded the qualified spelling
(`NativeCall::Types::Pointer<NULL>`, from the `NATIVECALL_POINTER_PRELUDE`'s
`method gist` in `src/runtime/run.rs`), while the *type object*'s `.^name` reported the
bare one. Verified on `main` before this change:

```
$ mutsu -e 'use NativeCall; my $p = Pointer[uint8].new; say $p.gist; say $p.^name'
NativeCall::Types::Pointer<NULL>
Pointer[uint8]
```

The investigation (`todo/deep/nativecall-types-package-qualification.md`, two prior
research passes) found the fix is not a mechanical rename. Two candidate strategies exist,
with genuinely different risk profiles.

### Strategy 1: display-only qualification

Keep every registry/Symbol key bare. Add one shared "qualify this builtin type's display
name for a human" helper, and call it from every place that stringifies a `Package`/
`Instance` type name for a human: `.^name`, `.raku`, and error-message type naming. Do
**not** call it from identity/dispatch comparison sites (`===`, `.isa`, MRO walks) or from
the ~15 sites listed below that do exact/literal string matching on the bare name.

### Strategy 2: real qualified registry key

Actually register `Pointer`/`CArray`/etc. under `NativeCall::Types::Pointer` etc. (mirroring
the existing `class GLOBAL::NativeCall::CStr` precedent in the same prelude), with a
`constant Pointer = NativeCall::Types::Pointer;` alias for backward compatibility. This
matches Rakudo's actual architecture: there would be exactly one registry key, holding the
name Rakudo itself gives it.

## Verification performed before deciding (2026-08-20, current `main`)

Before trusting the two-year-old-feeling investigation, its central claims were
re-verified against current code, not assumed:

**The `Pointer[uint8]` parametrization trap (Finding 3) still reproduces.**
`Foo[Args]` on a type object is not preserved source text; it is an `Index` AST node
evaluated at runtime, and when the target resolves to `ValueView::Package`,
`src/vm/vm_var_index_ops.rs` builds the parametrized name from the *already-resolved*
symbol:

```rust
Value::package(Symbol::intern(&format!("{}[{}]", name, args)))
```

So qualifying the registry key would make ordinary `Pointer[uint8]` — used pervasively in
`t/` and by the real OpenSSL/`IO::Socket::SSL` battery binding's `CArray`/`Pointer`
marshalling (`news/2026-07/openssl-battery-https.md`) — evaluate to
`"NativeCall::Types::Pointer[uint8]"`.

**The ~15 "not qualified-safe" call sites are still accurate.** Spot-checked directly on
current `main`:

- `src/runtime/runtime_class_query.rs::is_non_parametric_type` — literal
  `matches!(name, ... | "Pointer")`, no `::`-stripping. This is the allow-list that makes
  `Pointer[T]` legal syntax at all.
- `src/runtime/methods_aggregate_ctor.rs` — `base_class_name == "CArray"` (multiple sites).
- `src/runtime/nativecall_fnptr.rs:131` — `if base == "CArray"`.
- `src/runtime/native_types.rs` — literal `"long"`/`"ulong"`/`"longlong"`/`"ulonglong"`/
  `"size_t"`/`"ssize_t"`/`"bool"` throughout (bounds, width, signedness tables).
- The full list (~15 sites) is preserved verbatim in the closed-out `todo/deep/` finding's
  git history; every one checked still matches literally, unqualified.

Also re-verified: only four of the ten names are backed by a real registered class at all
(`Pointer`, `void`, `OpaquePointer` as a `constant` alias of `Pointer`, and
`NativeCall::CStr`, which is *already* registered under its real qualified key and needed
no change). The other seven (`CArray`, `long`, `longlong`, `ulong`, `ulonglong`, `size_t`,
`ssize_t`, `bool`) have no registry entry whatsoever; they resolve through a single
VM-wide bareword-term fallback (`is_builtin_type` in `src/vm/vm_value_helpers.rs`) shared
with unrelated core native-int type names (`int8`, `uint32`, ...). Giving these real
qualified class identity under Strategy 2 is not "rename a prelude class" — it is inventing
registration for names that currently have none, while being careful not to disturb the
native-int machinery the same fallback list serves for unrelated reasons.

No evidence turned up that Strategy 2's prerequisites have become cheaper since the
ticket was written — no `::`-stripping normalization pass has landed at any of the ~15
sites, and the `^add_method` short-name-downgrade workaround in
`methods_classhow_dispatch.rs` (which would need re-examining under Strategy 2, since
qualifying the registry key inverts the direction of the mismatch it currently patches) is
still in place, unchanged.

## Decision

**Strategy 1: display-only qualification.** Registry keys for all ten names stay bare.
Exactly one shared helper resolves a bare registry key to its human-facing display name;
it is called from `.^name`, `.raku`, and error-message type naming, and from nowhere else.

### Why, given CLAUDE.md's "refactor boldly" principle

CLAUDE.md is explicit that a temporary CI/roast failure is not a reason to avoid an
architecturally-correct change, and that over-caution avoiding the real fix is a worse
failure than a bold change CI catches. That principle does not clearly favor Strategy 2
here, because Strategy 2's risk is not "CI will show some red for a while and I'll fix it
forward" — it is that **ordinary, unqualified, everyday `use NativeCall` code**
(`Pointer[uint8]`, `CArray[uint8]`, exactly what the real OpenSSL/`IO::Socket::SSL`
battery binding depends on for its `https://` GET) silently starts evaluating to a
different value at ~15 literal-comparison call sites, with no compiler error to catch it —
a correctness regression in working user code, not a controlled, deterministically-caught
test failure. That is the specific failure mode CLAUDE.md's own "gain and risk" section
calls out as risk: "reducing Raku compatibility" for real programs, not a flaky test.

Strategy 1's honest cost is narrower and named explicitly rather than glossed over (see
"Consequences" below): mutsu's `Value::Package` model has exactly one registry key per
type, so **there is no meaningful mutsu statement of the form "`Pointer` and
`NativeCall::Types::Pointer` are distinct Package values"** — unlike real Rakudo, where a
lexical alias and its qualified original are, in principle, distinguishable by declaration
site even though they resolve to the same class. mutsu already collapses that distinction
today (this is exactly the pre-existing `Value::Package(Symbol)` representation described
in ADR-0047's Context section — "a Raku type object is represented as a bare name" with no
handle to a declaration site), so Strategy 1 does not introduce a new kind of divergence;
it just makes an existing one do double duty for one more thing (display qualification, not
just short-vs-mangled names).

If Strategy 2 becomes clearly tractable later — e.g. if the `::`-stripping normalization
pass across the ~15 sites gets done for an unrelated reason, or if `Value::Package` grows a
declaration-site handle (the "successor decision" ADR-0047 names and explicitly defers) —
re-open this decision as a new ADR rather than editing this one.

## Implementation

One helper, `qualify_nativecall_type_name`, added next to the existing
`user_facing_type_name` in `src/value/display.rs` (the function `.^name`'s fallback path,
`.raku`'s nested-element renderer, and `what_type_name` already used or were changed to
use for demangling lexically-scoped class names — see ADR-0047's "presentation layer
already handles this" note for the precedent of centralizing display-name logic there
rather than duplicating it per call site):

```rust
const NATIVECALL_TYPE_NAMES: &[&str] = &[
    "Pointer", "CArray", "void", "long", "ulong", "longlong", "ulonglong", "size_t",
    "ssize_t", "bool",
];

fn qualify_nativecall_type_name(base: &str) -> Option<String> {
    let split_at = base.find('[').unwrap_or(base.len());
    let (head, rest) = base.split_at(split_at);
    if NATIVECALL_TYPE_NAMES.contains(&head) {
        Some(format!("NativeCall::Types::{head}{rest}"))
    } else {
        None
    }
}
```

`user_facing_type_name` calls it after the existing decl-id-demangling and anonymous-type
steps, so a `Pointer[uint8]` parametrized display name is qualified with the `[uint8]`
suffix preserved, while the registry key it was derived from (`"Pointer[uint8]"`) is
untouched.

Three call sites were changed to route through this (all others already called
`user_facing_type_name` and needed no change, since the qualification lives in the shared
helper):

- `src/value/types.rs` (`what_type_name`) — both the `ValueView::Package` and
  `ValueView::Instance` arms, since error messages exercise both (a type-object mismatch
  goes through `Package`; a constructed `Pointer.new` value passed where a different type
  is expected goes through `Instance`).
- `src/builtins/methods_0arg/raku_repr.rs` (`raku_value`, the nested-element `.raku`
  renderer used e.g. when a `Pointer` appears inside an array being `.raku`'d) — was
  reading `name.resolve()` raw, bypassing even the demangling `user_facing_type_name`
  already did for lexically-scoped classes.

`.^name` (`methods_classhow_dispatch.rs`'s `"name"` arm) and the top-level `.raku`/`.gist`
method dispatch (`dispatch_core_repr.rs`) already routed through `user_facing_type_name`
before this ADR, so they picked up the qualification automatically once the helper itself
changed — no separate edit needed there.

`OpaquePointer` needs no entry in `NATIVECALL_TYPE_NAMES`: it is
`constant OpaquePointer = Pointer;` (a lexical alias, not a distinct registry key), so it
already resolves to the bare Symbol `"Pointer"` before this function ever sees it — and
`OpaquePointer === Pointer` was verified to still hold (`True`) after this change, since
identity comparison never touches this helper.

`NativeCall::CStr` needs no entry either: it is already registered under its real
qualified key and was unaffected before and after.

### What was deliberately left alone

`.gist` on a *type object* (not an instance) already special-cases short display via a
`rsplit("::").next()` step in `dispatch_core_repr.rs`, matching Rakudo's own behavior of
gisting an undefined type object as `(ShortName)` regardless of package
(`(void)`, `(CArray)`, `(long)` — verified identical against `raku` both before and after
this change). This path was not touched; it already produces the right output once the
underlying `user_facing_type_name` starts returning the qualified string, because
`rsplit("::")` strips it back down to the short form for `.gist` specifically.

The `.gist` on a `Pointer.new` *instance* still hardcodes
`'NativeCall::Types::Pointer<NULL>'` in the Raku-source prelude
(`src/runtime/run.rs`) without embedding the `[T]` parametrization
(`Pointer[uint8].new.gist` still prints `NativeCall::Types::Pointer<NULL>`, not
`NativeCall::Types::Pointer[uint8]<NULL>` as real Rakudo does). This is a pre-existing gap,
unrelated to the qualification mismatch this ADR fixes (that gist string was already
correctly qualified before this change — the mismatch was `.^name` being *bare*, not the
gist being wrong) and out of scope here; `Pointer`'s element-type tracking through a
parametrized `.new` is a separate, deeper feature.

### Not touched (identity/dispatch correctness)

- `src/value/types_isa.rs` (`===`/`.isa`/MRO-walk) — reads the bare Symbol directly, as
  before.
- The ~15 exact-match sites cataloged in the closed-out `todo/deep/` finding
  (`is_non_parametric_type`, `native_types.rs`, `nativecall_fnptr.rs`,
  `methods_aggregate_ctor.rs`, etc.) — all still compare against the bare, unqualified
  registry key, unchanged.
- `src/runtime/runtime_module_exports.rs` (`register_nativecall_exports`) — the
  `NativeCall::EXPORT::ALL` import-map introspection list, orthogonal to class
  registration.

## Consequences

- `.^name`, `.raku`, and error-message type naming for these ten NativeCall types now
  match real Rakudo's qualified spelling, closing the standing `.gist`-vs-`.^name`
  self-inconsistency this ADR opened with.
- This is a **deliberate, permanent divergence from Rakudo's actual object model**:
  `Pointer =!= NativeCall::Types::Pointer` as distinct Package *values* is not a
  meaningful statement in mutsu (there is only ever one registry key, `"Pointer"`), even
  though it displays as the qualified name. A program that somehow depended on the
  registry key itself being the qualified string (e.g. introspecting `WHO`/`HOW` storage
  directly, or comparing `.^name` output back into `::(...)` symbolic lookup) would not see
  what real Rakudo's registry contains. This tradeoff is accepted as the cost of avoiding
  Strategy 2's correctness trap on ordinary parametrized-type usage; no code doing that
  kind of introspection is known to exist in the batteries or `t/` today.
- Five existing `t/` assertions hard-pinned the bare spelling and were updated to the
  qualified one, which is a correction toward matching real Rakudo, not a workaround:
  `t/nativecall-type-surface.t` (lines 17, 32, 47, 73) and `t/nativecall-pointer.t`
  (line 63).
- If mutsu's `Value::Package` representation later grows a declaration-site handle (the
  end state ADR-0047 names and defers as its own "successor decision"), that would be the
  natural point to revisit whether NativeCall's types should become Strategy 2's real
  qualified registry key instead — tracked as a possible successor ADR, not scheduled here.
