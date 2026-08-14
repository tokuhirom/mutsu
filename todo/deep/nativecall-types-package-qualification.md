# Qualifying NativeCall's `Pointer`/`CArray`/etc. under `NativeCall::Types::*` needs a design pass, not a mechanical rename

(Escalated 2026-08-14 from `todo/tickets/nativecall-pointer-short-name.md`, which framed this
as "one deliberate slice" touching "at least" 3 call sites. A full investigation — two read-only
research passes over the whole `src/` tree — found the real blast radius is much larger, and one
of the two viable implementation strategies has a correctness trap that would break ordinary
`use NativeCall` user code, not just qualified-name edge cases. Recorded here per this repo's
policy of stopping and writing up a `todo/deep/` finding rather than forcing a large, risky change
through as a single PR.)

## The original problem statement (still true)

```
$ raku  -e 'use NativeCall; say Pointer.^name; say Pointer[uint8].^name'
NativeCall::Types::Pointer
NativeCall::Types::Pointer[uint8]

$ mutsu -e 'use NativeCall; say Pointer.^name; say Pointer[uint8].^name'
Pointer
Pointer[uint8]
```

Real Raku registers `Pointer`, `CArray`, `long`, `longlong`, `ulong`, `ulonglong`, `size_t`,
`ssize_t`, `bool`, `void`, `OpaquePointer` under the `NativeCall::Types` package and `use
NativeCall` imports the short names as aliases, so `.^name` on the type object carries the full
package path. mutsu reports the bare short name for all 11. There is also a standing inconsistency
already present in mutsu: `.gist` of an *instance* hardcodes the qualified spelling
(`NativeCall::Types::Pointer<NULL>`, `src/runtime/run.rs:48-53`), while `.^name` of the *type
object* reports the bare one — the two disagree with each other today.

## Finding 1 — these 11 names do not share one mechanism; most of them have no real class at all

Only `Pointer`, `void`, `OpaquePointer`, and `NativeCall::CStr` are real registered classes. They
come from `NATIVECALL_POINTER_PRELUDE` (`src/runtime/run.rs:36-73`), a block of literal Raku source
text spliced into every compunit that mentions `NativeCall` (`inject_nativecall_prelude`,
`src/runtime/run_prelude.rs:33-62`) and registered through the ordinary class-registration path
(`class GLOBAL::Pointer { ... }`, `class GLOBAL::void {}`, `constant OpaquePointer = Pointer;`).
**`class GLOBAL::NativeCall::CStr` in that same prelude is already the working precedent for the
target shape** — a class declared under a qualified name via `GLOBAL::Qualified::Name`, with no
bare alias (Rakudo doesn't export a bare `CStr` either, so mutsu doesn't need one).

`CArray`, `long`, `longlong`, `ulong`, `ulonglong`, `size_t`, `ssize_t`, and `bool` have **no
registry entry whatsoever**. When one of these barewords appears as a term (`say long.^name`,
`CArray.new`), it resolves through a single generic VM-wide bareword-term fallback:
`src/vm/vm_var_get_ops.rs:228` (and an earlier duplicate guard around line 105/115/161 for the
`Nil`-placeholder case):

```rust
} else if Self::is_builtin_type(name) || Self::is_type_with_smiley(name, self) {
    Value::package(Symbol::intern(Self::resolve_type_alias(name)))
```

`is_builtin_type` (`src/vm/vm_value_helpers.rs:308`, matches at lines 419-464) is a giant literal
`matches!` list that happens to include these 8 names alongside ordinary *core* native-int types
(`int8`, `uint32`, `num64`, ...) that have nothing to do with NativeCall. There is no
NativeCall-specific registration step at all — the Value is manufactured on the fly as
`Value::package(Symbol::intern("long"))` (etc.) with no backing class, no `type_metadata` entry,
nothing. `src/builtins/builtin_type_catalog.rs:111`'s `row!("CArray", ...)` looks like it might be
that missing registration, but it isn't consulted by the `.^name` path at all — it's read only by
`receiver_class.rs`/`registry.rs` for MRO/dispatch-owner classification (confirmed by tracing every
caller of `builtin_type_info()`).

**Implication:** giving these 8 names a real qualified package identity is not a rename of an
existing class — it would mean inventing class/type registration for names that currently have
none, while being careful not to disturb the *many* other native-int type names (`int8`, `uint32`,
...) that share the exact same fallback list and match arms for unrelated reasons (native-int
signature typing, array element typing, native-int bounds/wrapping — none of which is
NativeCall-specific and none of which should gain a `NativeCall::Types::` prefix).

## Finding 2 — `.^name` display is not centralized; a display-only patch creates new inconsistencies rather than removing the existing one

The type-object name meta-method (`.^name`) is computed in
`dispatch_classhow_method`'s `ValueView::Package(name)` arm
(`src/runtime/methods_classhow_dispatch.rs:182-189`):

```rust
ValueView::Package(name) => self
    .type_metadata
    .get(&name.resolve())
    .and_then(|m| m.get("__set_name__"))
    .map(Value::to_string_value)
    .unwrap_or_else(|| {
        crate::value::user_facing_type_name(&name.resolve()).to_string()
    }),
```

But at least **three other independent call sites** stringify the same `Package`'s `Symbol`
directly, without going through this function or `user_facing_type_name`:

- `.raku` — `src/builtins/methods_0arg/raku_repr.rs:927`: `ValueView::Package(name) =>
  name.resolve().to_string()`. Already bypasses even the `__set_name__` override that `.^name`
  respects, so `.raku` and `.^name` can already disagree for a class that calls `.^set_name`.
- Error-message type naming (`X::TypeCheck` and friends) — `what_type_name` in
  `src/value/types.rs:48`: `ValueView::Package(name) => name.resolve()`, raw, no qualification
  layer at all.
- Identity/dispatch comparisons — `src/value/types_isa.rs` (lines 10, 73, 159, 284, 299, 346, 369,
  427) read `name.resolve()` directly for `===`/`.isa`/MRO-walk purposes.

A patch that only teaches `dispatch_classhow_method`'s Package arm to show a qualified name for
these 11 keys (leaving the underlying Symbol/registry key bare) would fix `.^name` but leave
`.raku`, error messages, and `===`/`.isa` semantics *still* showing/comparing the bare spelling —
trading today's one known inconsistency (instance `.gist` qualified vs. type `.^name` bare) for a
*wider* one spanning four independently-maintained surfaces. That is exactly the "cosmetic
dressing instead of a genuine fix" pattern this repo's working agreements rule out, so it is not an
acceptable resolution on its own — it would need matching edits at all four sites to be an honest
fix, which is already more than "one deliberate slice" once written out explicitly.

## Finding 3 — the "real" fix (qualify the actual registry key) has a correctness trap that breaks ordinary user code, not just qualified spellings

The alternative — actually register `Pointer`/`CArray`/etc. under the qualified key (`class
GLOBAL::NativeCall::Types::Pointer { ... }` + `constant Pointer = NativeCall::Types::Pointer;`,
mirroring the `NativeCall::CStr` precedent and the existing `constant OpaquePointer = Pointer;`
alias already in the prelude) — was verified empirically to hit a specific, easy-to-miss bug:

```
$ mutsu -e 'use NativeCall; constant MyPointer = Pointer; my $p = MyPointer[uint8].new; say $p.^name'
Pointer[uint8]
```

`Foo[Args]` on a type object is **not** parsed as literal source text preserved verbatim — it's an
`Index { target: BareWord("Foo"), index: ... }` AST node evaluated at runtime. When the target
resolves to a `ValueView::Package`, `src/vm/vm_var_index_ops.rs:2246` builds the parametrized type
name from the *already-resolved* symbol, not what the user typed:

```rust
Value::package(Symbol::intern(&format!("{}[{}]", name, args)))
```

So once `Pointer`/`CArray` are registered under the qualified key, **completely ordinary,
unqualified, everyday `use NativeCall` code** — `Pointer[uint8]`, `CArray[uint8]`, used pervasively
across `t/` and in this repo's real bindings (the OpenSSL + `IO::Socket::SSL` battery specifically
depends on `CArray`/`Pointer` marshalling for its `https://` GET, per
`news/2026-07/openssl-battery-https.md`) — starts evaluating to
`Value::package("NativeCall::Types::Pointer[uint8]")` / `"...CArray[uint8]"`. Every call site below
that compares a parsed-out base class name to the literal short string **without stripping a `::`
prefix first** would then silently stop matching, for ordinary code, not just code that explicitly
spells out the qualified name — the ADR-0015 §2.1 failure mode the original ticket warned about,
except firing on everyday usage rather than an edge case.

### Sites already qualified-safe (strip to the last `::` component before comparing) — no change needed

- `src/runtime/cstruct_layout.rs`: `short_base_name()` (175-181) and its callers — `FieldType::from_type_name` (63), `try_pointer_method` (887), `try_native_handle_repr_where` (813-821)
- `src/runtime/nativecall_global.rs:104-109`
- `src/runtime/builtins.rs:741-742` (via `cstruct_layout::short_base_name`)
- `src/builtins/methods_0arg/dispatch_core_coerce.rs:553-563`
- `src/value/value_carray.rs:48-55` (`carray_elem_type_name`; its own unit test already exercises `"NativeCall::Types::CArray[uint16]"`)

### Sites NOT qualified-safe — literal/exact matches that would break

- `src/runtime/runtime_class_query.rs::is_non_parametric_type` (69-108) — `matches!(name, ... | "Pointer")`, no `::` stripping. This is the allow-list that makes `Pointer[T]` legal syntax at all.
- `src/runtime/types/type_matching.rs` (581, 594-596, 613, 634, 650) — `constraint == "CArray"`, `match base { "array" => ..., "CArray" => ..., "Pointer" => ... }`, fed by `parse_generic_constraint()` (`src/runtime/types/mod.rs:745-756`) which splits on `[` without `::`-stripping.
- `src/vm/vm_register_sub_ops.rs:829` — `if tc == "CArray"` after `resolve_native_type_alias()`.
- `src/runtime/nativecall_fnptr.rs:131` — `if base == "CArray"` after the same alias walk.
- `src/runtime/methods_aggregate_ctor.rs` (222, 233, 278, 283, 452) — `base_class_name == "CArray"`, `matches!(base_class_name, "Array" | "array" | "CArray")`.
- `src/runtime/methods_object_dispatch_new.rs` (641-653, 765, 1252) — same `base_class_name` chain.
- `src/runtime/methods_classhow_dispatch.rs:634-659` (`^add_method`) — already contains a workaround for the *reverse* direction today (downgrades an explicitly-qualified `NativeCall::Types::Pointer.^add_method(...)` to the short registered name); qualifying the registry key inverts this and needs the workaround re-examined, not just left in place.
- `src/runtime/nativecall.rs:60-77` (`CType::from_type_name`) — safe for `Pointer`/`OpaquePointer` (recognized before any alias walk), **unsafe for `CArray`** (not in this match at all, so it falls through to alias resolution and would come back qualified).
- `src/runtime/native_types.rs` (`NATIVE_INT_TYPES` and all its match arms, lines 8-141) — literal `"long"`/`"ulong"`/`"longlong"`/`"ulonglong"`/`"size_t"`/`"ssize_t"`/`"bool"` throughout.
- `src/value/value_buf.rs` (103, 106, 110) — same category.
- `src/runtime/utils/type_constraints.rs` (`is_known_type_constraint`, 1-12; `"CArray"` at 117) — `constraint.split('[').next()`, no `::` stripping (lower risk, usually fed literal parser text, but not universally).
- `src/runtime/undeclared_routines.rs::NATIVE_TYPE_NAMES` (39-64) — bareword-in-call-position allow-list (`long(5)`-style coercions); different concern, same 7 lowercase names.
- `src/vm/vm_value_helpers.rs::is_builtin_type` (419-464) — the bareword-term resolution allow-list itself (Finding 1).

`src/runtime/runtime_module_exports.rs:169-207` (`register_nativecall_exports`) is a separate,
probably-unaffected concern: it registers the bare names under `NativeCall::EXPORT::ALL` for
import-map introspection, which is orthogonal to class registration and should plausibly stay bare
regardless of which route is chosen — it *is* the import-alias list, not the class registry.

## Finding 4 — test impact is real but small and roast is essentially clear

Five `t/` assertions hard-pin the bare `.^name` today and would need updating to the qualified
spelling if this ships (which is also what real Rakudo actually reports, so the update is a
correction, not a workaround):

- `t/nativecall-type-surface.t:17,32,47,73`
- `t/nativecall-pointer.t:63`

`t/nativecall-pointer.t:21` already asserts the qualified `.gist` spelling (the standing
inconsistency this ticket set out to fix). `roast/` is essentially unaffected: only one file
(`roast/S17-procasync/windows-print-raw-args.raku`, Windows-only) references `CArray` alongside
`NativeCall`, and it has no `.^name`/`.gist`/error-message assertion — nothing there needs the
change.

## Recommendation

This needs a design decision (write it as an ADR, or at minimum a scoped proposal that names the
tradeoff explicitly) before implementation, because the two candidate strategies are genuinely
different architectures with different risk profiles:

1. **Display-only qualification.** Keep every existing registry/Symbol key bare (so parametrization
   via `vm_var_index_ops.rs:2246` and all ~15 exact-match sites above keep working completely
   unchanged), and add exactly one shared "qualify this builtin type's display name" helper,
   called from every place that currently stringifies a type name for a human
   (`.^name`, `.raku`, error messages) — NOT from the ~15 sites above, which must keep reading the
   real (bare) key for correctness. This is honest and low-risk, but is a deliberate, permanent
   divergence from Rakudo's actual object model (`Pointer =!= NativeCall::Types::Pointer` as
   distinct Package values would no longer be a meaningful statement in mutsu, since there'd only
   ever be one key). Needs an explicit decision that this divergence is acceptable, and the "give
   the NativeCall prelude its real package" framing in the original ticket should be dropped in
   favor of "make display consistent" as the actual scope.
2. **Real qualified registry key**, matching Rakudo's actual architecture. Requires, as
   prerequisites (not follow-up work): (a) normalizing every "NOT qualified-safe" site above to
   `::`-strip before comparing, extending the `short_base_name`/`rsplit("::").next()` convention
   that already exists at several sites to the ones that don't have it yet; (b) re-examining the
   `^add_method` short-name-downgrade workaround in `methods_classhow_dispatch.rs:634-659` now that
   the direction of the mismatch inverts; (c) deciding what happens to the ~7 primitive C-width
   aliases that currently have no class at all (Finding 1) — either give them real class identity
   too (a materially bigger change than "rename a prelude class"), or explicitly scope them out and
   accept that `long`/`bool`/etc. stay on the display-only strategy while `Pointer`/`CArray` get
   the real one; (d) a regression pass specifically exercising `Pointer[T]`/`CArray[T]` marshalling
   through the OpenSSL/IO::Socket::SSL battery (not just `.^name` unit assertions), since Finding 3
   shows the failure mode fires on ordinary parametrized-type usage, which is exactly what that
   battery depends on.

Either route also has to preserve `OpaquePointer === Pointer` identity (already true today via the
existing `constant OpaquePointer = Pointer;` alias — verify it survives whichever strategy is
picked) and reconcile the now-redundant `builtin_type_catalog.rs` `CArray` row once the real
mechanism producing `.^name` is settled.

## Bigger, tracked separately (unchanged from the original ticket)

- ~~**`is native` on methods**~~ — **done 2026-07-29**, see
  [`news/2026-07/nativecall-cglobal-and-native-methods.md`](../../news/2026-07/nativecall-cglobal-and-native-methods.md).
- ~~**`nativecast` tags handles with the short class name**~~ — **done
  2026-07-29**, see
  [`news/2026-07/cstruct-handles-carry-their-registered-name.md`](../../news/2026-07/cstruct-handles-carry-their-registered-name.md).
- **By-value CStructs and callbacks** — recorded in `runtime/nativecall.rs`'s
  module docs as follow-up work; no dist in the batteries needs them yet.
  (Argument-position CStructs work: a struct is an opaque native handle passed
  by pointer, which is what made the genuine OpenSSL + IO::Socket::SSL binding
  run a real `https://` GET — see
  [`news/2026-07/openssl-battery-https.md`](../../news/2026-07/openssl-battery-https.md).
  What is missing is **by-value** field-layout marshalling and generic C
  callbacks.)
- **A *returned* `CArray[T]` is surfaced as the raw `Pointer` it carries** —
  there is no length with which to reify a Raku array, so the return is handed
  back as the pointer instead of an indexable `CArray`. Rakudo returns a
  `CArray` you can index (reading past the end is the caller's problem).
- **Native-backed `array[T]` (ADR-0015 P3b) and reference-element
  `CArray[Str]`/`CArray[Pointer]` (P3c)** are tracked in
  [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md),
  not here. P3b also fixes `array-shapes.t` T36-38.

## Reproducing the original observation

```sh
for t in long longlong ulong ulonglong bool size_t ssize_t void CArray Pointer OpaquePointer; do
  printf '%-14s mutsu=' "$t"
  mutsu -e "use NativeCall; say $t.^name" 2>&1 | head -1
done
```
