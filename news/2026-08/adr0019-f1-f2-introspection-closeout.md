# ADR-0019 Phase F boxes F1/F2 closed: introspection reads the canonical table

`todo/deep/adr0019-f1-f2-introspection-canonical-source.md` scoped Phase F's two introspection
boxes on 2026-08-14: F1 ("build `Method` objects from canonical entries") and F2 ("derive
`.^methods`/`.^can`/method MRO from the resolver/table"). Both are now closed in
`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md` — this note closes the
ticket and consolidates the pieces that landed without their own news entry, alongside the ones
that already have one.

## Item 1 — user methods: shadow-then-cutover, zero mismatches

`.^methods`/`.^method_table`/`.^can`/`.can` used to build their candidate lists by walking
`ClassDef::methods` directly, a separate read path from the canonical
`Registry::method_entries[(owner, name)].user_candidates` table Phase E's dispatch already used
exclusively. The E1a shadow-then-cutover pattern applied cleanly:

- **`.^methods`/`.^method_table`**: shadow-check (#6399) found zero discrepancies across `make
  test` plus a broad roast sweep, then the cutover (#6400) switched `collect_class_methods`/
  `class_method_table` to read `user_candidates` and deleted the shadow-check instrumentation.
- **`.^can`/`.can`**: same pattern, `collect_can_methods` (shadow-check #6402, cutover #6406),
  zero mismatches across a full `t/` sweep plus `roast/S12-introspection/{can,meta-class,walk}.t`,
  `roast/S12-enums/thorough.t`, `roast/S32-exceptions/misc2.t`.

The sync-fidelity assumption the whole slice rested on — that
`Registry::sync_user_method_entries` mirrors every `ClassDef::methods` mutation site — held
empirically with no missing sync call found. Pins: `t/classhow-methods-package.t`,
`t/can-methods-drift.t`.

## Item 2 — native methods: mechanism slice landed, fidelity slice deliberately parked

`make_native_method_object` was a stub (`is_dispatcher: False`, empty `params`, `Mu`/`Mu`
returns/of) — every native method's `.^lookup("name").signature` reported zero parameters
regardless of real arity, confirmed against real `raku` before any fix. Consulted with the user
2026-08-14: extend the one E2-generated catalog (`NativeMethodRow`) with optional fidelity
columns rather than hand-authoring a second `(owner, name)` table — F3's "no second source of
truth about which methods exist" ban stays intact, since declaration-signature fidelity for a
native method has no in-repo derivation to duplicate (the same way `MethodDef::param_defs` is the
only source for a user method's signature).

Landed in three slices, all raku-verified:

- **`.package`** (`news/2026-08/adr0019-f1-method-package-mechanism-slice.md`): user/role
  methods get their exact declaring type; native methods default to the catalog `owner` (an
  accepted imperfect default — e.g. `Str.uc` answers `(Str)`, not Rakudo's true `(Cool)`).
- **`.signature`**: `make_native_method_object` now calls
  `synthesize_native_signature(owner)` instead of building an empty `Signature()`. A raku
  ground-truth sweep of ~280 introspectable (owner, name) pairs found no single shape dominates
  real Rakudo's native signatures and no pattern is derivable from arity alone, so this
  synthesizes the observed plurality shape — `(Owner $:: |)` — as a generic default. Not exact
  Rakudo parity by design. Along the way, fixed a latent `render_signature` bug (a hand-built
  `;;`-free `SigInfo` needs `multi_invocant: true` on every param, not just the invocant, per the
  parser's own semantics for that field) and filed a separate pre-existing bug,
  `todo/tickets/signature-arity-count-wrong-for-capture-params.md` (`Signature.arity`/`.count` are
  wrong for any `is_capture` param, reproducing on plain user-declared subs too). Pin:
  `t/classhow-native-method-signature-default.t`.
- **Sub-vs-Instance unification** (`news/2026-08/classhow-lookup-method-instance-unification.md`):
  `.^lookup`/`.^find_method` used to return a `Sub`-shaped value, a different representation from
  the `Method` `Instance` `.^methods`/`.^can` build, so `Method`-only accessors
  (`.is_dispatcher`, `.multi`, `.candidates`) were unreachable on a `.^lookup` result. Now unified
  on the `Instance` shape, kept callable via a `CALL-ME` handler. Found and fixed five real bugs
  along the way (missing invocant in `Method.signature`, `.candidates` on a non-multi method,
  cross-class multi-family `.candidates`, `.WHY` on a Method Instance, dynamic hyper dispatch on a
  method value).

**What is intentionally not done**: exact native-method `.package`/`.signature`/
`.is_dispatcher`/`.multi` fidelity (matching Rakudo's true declaring type and signature shape
per method) requires per-method hand data — a `.package` divergence sweep across 9 representative
owners found 199 divergent triples just from those owners (extrapolated: 400+ across all ~650
introspectable rows), confirming this must stay a *reactive* fidelity slice (add an override only
when a real `t/`/roast assertion demands it), never an upfront sweep — populating it speculatively
would be exactly the "second hand table" shape ADR-0019 Phase F retires elsewhere
(`builtin_type_methods.rs`'s per-type name arrays, F3).

Spot-checked again at closeout (2026-08-20) against real `raku`: `(42).^lookup("floor").signature`
and `"abc".^lookup("uc").package` both still show the generic mechanism-slice defaults rather than
Rakudo's exact answers, exactly as documented above — and `(42).^lookup("Numeric").is_dispatcher`
now returns `False` instead of crashing (an improvement from the Sub-vs-Instance unification) but
is not yet the real `True` a native multi method's dispatcher should answer. Both remain the
correctly-idle fidelity gap, not a regression.

## Where this leaves ADR-0019

ADR-0019 closed Accepted/Implemented on 2026-08-17 (completion gate G4): F1/F2's remaining
fidelity slice was judged explicitly non-gating, alongside E2's exact-handler-ID catalog and a
few other deliberately-parked cleanup items. The residual is now tracked directly in the ADR's
own F1 box text rather than a separate `todo/deep` file, since the ADR already carries the full
decision record and stays live for future sessions to read.
