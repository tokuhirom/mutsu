# ADR-11827: A routine has identity; `does` on it composes in place

- **Status**: Proposed (maintainer asked for this ADR on 2026-10-04). Phase 1 is implemented; see §5.
- **Date**: 2026-10-04
- **Deciders**: tokuhirom, Claude
- **Issue**: [#11827](https://github.com/tokuhirom/mutsu/issues/11827)
- **Related**: [ADR-11203](11203-nativecall-runs-upstream-via-the-backend-neutral-path.md) (upstream
  NativeCall, whose `is native is symbol` and `is native` methods depend on this),
  [ADR-0060](0060-mixin-what-is-a-composition-keyed-type-object.md) (the composition-keyed `.WHAT` cache),
  #11479 (`Code.$!do` rebinding)

## 1. Context

In rakudo a `Routine` is an object. `$r does R` reblesses that object, so every alias of it
sees `R`:

```raku
role R { method hi { 'hi' } }
sub f { }
my $g = &f;
&f does R;
say $g ~~ R, ' ', $g.hi;   # raku: True hi
```

mutsu gives instances this identity already: `does_rebless_instance` reblesses the shared
attribute cell. Routines have no such cell. Today:

- `does` on a Sub returns a new `Value::Mixin(Sub, overrides)` and rebinds only the named
  variable it was written on. The trait loop writes that value to `env["&name"]`. An alias
  taken earlier still holds the plain Sub, and so does a role argument that captured the routine
  (`$g ~~ R` is False above).
- A partial, name-keyed patch exists: `ROUTINE_MIXIN_ROLES` (`"pkg::name" → role names`,
  `src/runtime/registration_sub.rs`). Rebuilding `&name` from the registry re-attaches the role
  *markers* only. Role arguments and role attribute state are not restored, and an anonymous
  routine is not covered at all.
- Method traits (`apply_method_is_traits_inner`) apply each trait to a throwaway Sub and discard
  the result. A role or a `$!do` rebinding a trait puts on a method never reaches the method
  that dispatch calls.

Upstream NativeCall needs exactly this identity. The `t/` failures on the switch branch show it
(measurement 6 on #11203, about 13 files):

- `is native is symbol('qsort')`. The native trait runs `$r does Native[$r, $lib]`, so the
  role keeps `$r` as `$routine`. The symbol trait then runs `$r does NativeCallSymbol['qsort']`.
  `Native!setup` calls `$routine.?native_symbol` and, in mutsu, reaches the copy taken before
  the second `does`. It therefore looks up the routine's own name.
- `method m(...) is native`. Neither the `Native` role nor its `$!do` replacement reaches the
  installed method, so the `{ * }` body runs.

## 2. Decision

### 2.1 A routine composition cell

Every routine gets one shared **composition cell**: a lock-protected slot holding the
`Gc<MixinOverrides>` of the roles composed into that routine so far. It also has a cheap
"has composition" flag, so a routine that was never mixed into pays one relaxed atomic load.

- `SubData` carries the cell as an `Arc`. Every value of one routine holds the same `Arc`:
  internal `SubData` clones (env tweaks, callable-type tags), a `Mixin` wrapper's inner Sub, and
  the code objects rebuilt from the registry (`sub_value_from_function_def`).
- A named routine's `FunctionDef` owns the cell. A rebuild copies the `Arc` from the def, so
  `&foo`, a trait handler's `$r`, a role argument that captured `$r`, and the registry entry are
  all one routine.
- A closure literal creates a fresh cell when it is created: each closure value is its own
  object, as in rakudo.

### 2.2 `does` writes the cell; routine receivers read it

- **Write.** `does` on a routine (a Sub, or a Mixin over one) first takes the routine's
  *current* composition from the cell. It composes the new role onto that, as today, and then
  stores the resulting overrides back into the cell. Successive `does` therefore accumulate, and
  role arguments and attribute state stay on one overrides node.
- **Read.** Wherever a routine is a method receiver or the subject of a role check (method
  dispatch, `.does`, `~~ Role`, `.^name` and `.WHAT`), a Sub or Mixin-over-Sub whose cell holds
  a composition is viewed as `Mixin(inner Sub, cell overrides)`. A stale alias and a fresh one
  then dispatch through the same overrides node, so a role attribute written through one is read
  through the other.
- **`but` is unchanged.** It builds a new object in rakudo too: it copies the routine, starting
  from its current composition.

### 2.3 `.clone`

A Raku-level `.clone` of a routine is a new object. It gets a **fresh** cell, seeded with a copy
of the original's current overrides; a later `does` on either one does not reach the other.
Internal `SubData` clones keep the shared cell (§2.1).

### 2.4 Methods (phase 2)

A method declaration's traits are applied to **one** persistent `Method` code object, threaded
through every trait handler in order, as the named-sub loop already threads `$r`. That object's
cell is the method's identity: it is attached to the method's registry definition, so
`.^find_method`, `.^methods` and dispatch see the same composition. A `$!do` rebinding on that
object becomes the method's body for dispatch: it lands in the method's own wrap chain (the
`(class, method, candidate)` chain `.wrap` already uses), with the replacement invoked with the
invocant first.

### 2.5 Threads and the cycle collector

- The cell is shared across Raku threads, so it is a lock, not a `&self → &mut` access
  (docs/security.md): a racing `does` may see either composition, never a torn one.
- A role argument can be the routine itself (`Native[$r, …]`), so the cell can close a cycle.
  The cell is treated the way `SubData`'s *shared* env overlay already is: it is held by every
  value of the routine, so no single node owns its edges, and the collector does not trace
  through it. A cycle routed only through a cell is deferred, never corrupted. Named routines
  live as long as the registry anyway. A closure that is mixed into with itself as a role
  argument stays alive; the ADR accepts that as the conservative side.

### 2.6 `ROUTINE_MIXIN_ROLES`

The name-keyed table becomes redundant once rebuilds share the def's cell. It is removed in
phase 1 when every reader reads the cell, and kept as is until then.

## 3. Rejected alternatives

- **Re-point role arguments on the second `does`.** When composing onto `X`, replace any role
  argument identical to `X`'s inner Sub with the new result. This fixes only the
  self-reference shape, and the stale copy captured as a closure's `self` still diverges.
- **A registry keyed by Sub id holding the latest composed value.** It needs an Interpreter-level
  table (forbidden by ADR-10779 and the "no new fields on `Interpreter`" rule). It also never
  frees entries, and is a second name/id-keyed patch next to `ROUTINE_MIXIN_ROLES`.
- **Patching the vendored NativeCall** to read `self.?native_symbol` instead of
  `$routine.?native_symbol`. ADR-0096/ADR-11203 forbid patching vendored modules, and other
  user code relies on routine identity too.

## 4. Consequences

- `&f does R` is visible through every alias, the registry entry, role arguments and
  closures' `self`.
- One more `Arc` per `SubData`. Routines that are never mixed into pay one atomic flag load per
  dispatch, only where the receiver is a routine.
- A cycle through a cell is not collected (§2.5).

## 5. Implementation status

| Phase | Content | State |
| --- | --- | --- |
| 1 | Cell on `SubData` / `FunctionDef`; `does` writes it; method dispatch and role checks on routine receivers read it; `.clone` gets a fresh cell | done (`t/oo/routine-does-identity.t`) |
| 2 | Method traits on one persistent `Method`; `$!do` on a method reaches dispatch | planned |
| 3 | Remove `ROUTINE_MIXIN_ROLES` | planned |
