# ADR-0019 Phase F scoping: introspection still mirrors declarations, not the canonical entry table

Phase E (E1-E11) closed 2026-08-14 (PR #6390): every dispatch call site now routes through the
canonical `Registry::method_entries` table via `call_method_with_values`/`try_native_method`. Phase
F ("derive introspection and remove compatibility state") is the next open phase in
`docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`'s execution plan, boxes F1-F7,
none started. This is a scoping note, not a design — F1 needs its own raku-verification pass before
a design doc is safe to write, the same way E1's design needed the receiver-owner survey first.

## What F1/F2 ask for

- **F1** — "Build `Method` objects from canonical entries. Store ownership, visibility, signature,
  multi/submethod, wrap, and native metadata needed by introspection."
- **F2** — "Derive `.^methods`, `.^can`, and method MRO views from the resolver/table. Use the same
  TypeId MRO and visibility rules as calls."

## Survey: introspection is a separate read path today, in two different ways

`.^methods`/`.^method_table`/`.^lookup` (`src/runtime/methods_classhow_method_obj.rs`,
`collect_class_methods`/`class_method_table`) build `Method` `Instance` objects by walking
`ClassDef::methods: HashMap<String, Vec<MethodDef>>` and `ClassDef::native_methods: HashSet<String>`
directly — **not** `Registry::method_entries[key].user_candidates`/`.builtin`, the canonical table
Phase E's dispatch path now uses exclusively. Two distinct gaps, not one:

1. **User methods**: `MethodDef` (`runtime/decl_types.rs:95-140`) already carries everything F1
   wants — `param_defs`, `is_private`, `is_multi`, `is_submethod`, `return_type`, `deprecated_message`
   — and `Registry::method_entries[key].user_candidates: Vec<MethodDef>` is supposed to already
   mirror `ClassDef::methods` (the write side synced them starting Phase B, "canonical method-table
   write side"). If that sync is exact, cutting `collect_class_methods`/`class_method_table` over to
   read `user_candidates` instead of `ClassDef::methods` is a **shadow-then-cutover slice in the E1a
   style** — low risk, mechanical, verifiable with a diff-the-two-sources probe test before cutover.
   `Registry::sync_user_method_entries(class_name)` (`registry.rs:332-387`) does a full
   clear-and-reclone of `class_def.methods` into `user_candidates` (not an incremental patch), so
   fidelity only requires that every `ClassDef::methods` mutation site calls it afterward — grepping
   the 17 call sites shows coverage across class-body declaration/exit, role composition
   (`methods_object_dispatch_new.rs`), role-parameterization rename (`registration_class_compose_body.rs`,
   old-name **and** new-name), `augment` (`registration_class_augment.rs`), and `.^add_method`
   (`methods_classhow_dispatch.rs`) — no mutation path found missing a paired sync call on inspection,
   though this was a grep-and-read pass, not a runtime shadow-check. **A debug-assert shadow-check
   comparing `user_candidates` against `ClassDef::methods` across a full `make test` + `make roast`
   run (the E1a pattern) is still the right way to *prove* this empirically before cutover** — it is
   cheap (one assertion, no behavior change) and would have caught the E8b proto-entry drop the same
   survey style caught elsewhere.

2. **Native/builtin methods**: this is the real gap, and it is a genuine feature hole, not just a
   duplicate-path cleanup. `make_native_method_object` (`methods_classhow_method_obj.rs:144-159`)
   is a **stub**: hardcoded `is_dispatcher: False`, an empty `params` array, `returns`/`of` both
   `Mu`. Every native method's `.^lookup("name").signature` therefore always reports zero
   parameters regardless of the method's real arity — confirmed by reading the function, not yet
   confirmed against real `raku` output (needs `raku -e '(42).^lookup("floor").signature.gist'` or
   similar, compared to mutsu's answer). `BuiltinMethodEntry` (`builtins/builtin_type_methods.rs:721`,
   the E2-catalog row type Phase E built) is `{owner: &str, name: &str, order: u16}` — no signature,
   no arity, no visibility. `ClassDef::native_methods` is `HashSet<String>` — names only, same gap.
   To make F1's "signature ... native metadata" real, either (a) hand-author per-native-method
   arity/param-name metadata (the same shape of hand table F3 wants to *retire* elsewhere — a
   direct tension worth raising explicitly rather than silently re-growing what F3 shrinks), or
   (b) derive it structurally from the `native_method_{0,1,2}arg` arity cascade the method is
   dispatched through (arity is mechanical; parameter *names* are not, since Rust match arms don't
   carry Raku signature names) — needs a decision before any code.

## Why this needs its own raku-verification pass before a design doc

Every prior Phase E box that touched observable output (E1, E7 step4/5/6, E9-pre, E11 slice 2/5) found
real behavior gaps only by comparing against actual `raku` output first, not by reading the mutsu
source in isolation — e.g. E11 slice 2 found `can-ok "abc", "substr"` silently failing on mutsu despite
looking correct on paper. F1/F2 touch `.^methods`/`.^lookup`/`.gist` on `Method` objects, which is
exactly the kind of surface with subtle rakudo conventions (e.g. does `.^lookup("foo").signature` on a
native method show `(*@args)` slurpy in real rakudo, or the true fixed arity? does a multi native
method's `.candidates` exist at all?). A design written without that ground-truth table would repeat
the mistake E1's design explicitly called out avoiding.

## Suggested next step

A dedicated raku ground-truth session (E9-pre-style) for the ~10-15 native `Method`-introspection
questions above (signature shape, `.candidates` for multi natives, `.package`/`.name` on inherited vs.
own methods, wrap/multi/submethod flags), landing as pins under `t/`, before writing the F1 design
doc. The user-method half (item 1 above) can proceed independently and sooner — it is a mechanical
shadow-then-cutover slice once `user_candidates`'s sync fidelity is confirmed.

## Progress (2026-08-14)

Item 1's user-method cutover is done, across all three MRO/table readers that used to walk
`ClassDef::methods` directly:

- `collect_class_methods`/`class_method_table` (`.^methods`/`.^method_table`) — shadow-check
  #6399, cutover #6400.
- `collect_can_methods` (`.^can`/`.can`) — shadow-check #6402, cutover #6406, same
  shadow-then-cutover pattern, zero mismatches across a full `t/` sweep plus the
  `S12-introspection/{can,meta-class,walk}.t`, `S12-enums/thorough.t`, `S32-exceptions/misc2.t`
  roast files.

Item 2 (native/builtin method metadata) remains untouched and is still the real open work — the
raku ground-truth pass below narrows it further but does not resolve the F3-tension question in
item 2's option (a) vs (b).

**Raku ground truth gathered while scoping item 2** (`raku -e`, not yet mutsu pins under `t/`):

- Native-method `.signature.gist` is real Rakudo behavior, not a stub — but it is **not**
  arity-derivable in general: `(42).^lookup("floor").signature.gist` is
  `(Int:D $:: *%_ --> Int:D)` (generic named-catchall, no positional info at all despite `floor`
  taking zero args beyond the invocant); `"abc".^lookup("substr").signature.gist` is
  `(Cool $:: |)` (a raw capture, arity fully erased); `Array.^lookup("push").signature.gist` is
  `($:: |)`; `Hash.^lookup("push").signature.gist` is `(Hash $:: +values, *%_)` (this one *does*
  show a real slurpy-positional name). So Rakudo's own native signatures range from "fully generic
  capture" to "one specific slurpy param name" with no discernible single pattern — confirms this
  needs per-method hand data to match exactly (option (a)'s tension with F3 is real, not
  hypothetical), or an intentionally-generic placeholder (option (b), accepting it won't match
  Rakudo exactly since Rakudo itself isn't derivable from arity alone).
- `.is_dispatcher` is a real accessor Rakudo exposes on any `Method` (`$m.is_dispatcher` → `False`
  for `floor`, `True` for a multi's dispatcher Method) and `.multi` too (`True`/`False`). Checked
  against mutsu: **`.is_dispatcher`/`.multi` are unreachable on any `Method`-shaped value returned
  by `.^lookup`** (`(42).^lookup("floor").is_dispatcher` prints `<composed-method:is_dispatcher>`
  instead of `False`) — this is not an item-2-only gap, it reproduces on `.^lookup` of a plain user
  method too. Root cause found: `classhow_lookup`/`classhow_lookup_impl`
  (`methods_classhow_lookup.rs`) builds its return value as a **`Sub`-shaped** `Value::make_sub`,
  not the `Method`-`Instance`-shaped value `collect_can_methods`/`collect_class_methods` build via
  `make_method_object_with_owner`. Calling an unknown method on a `Sub` value falls into the
  callable-compose fallback in `methods_instance_ops.rs` (~line 2117), which silently returns a
  bogus composed-callable instead of erroring — so `.^lookup(...).is_dispatcher` (or any other
  `Method`-only accessor) never reaches a real answer. This is a distinct, smaller, well-scoped bug
  (two representations of "a method" that don't interoperate) — separate from F1/F2's native-metadata
  question and not blocked on the raku ground-truth session above. Filed as
  `todo/tickets/classhow-lookup-returns-sub-not-method-instance.md`.

## Progress (2026-08-14, second pass): `.package`, `.candidates`, and a gap #6420 didn't cover

More raku ground truth gathered for item 2, continuing the ~10-15-question list this file's
"Suggested next step" called for. No code changed this pass; findings only.

- **`.package` is not the concrete receiver's own type — it's wherever Rakudo core actually
  declares the method**, which is not mechanically derivable from a `(owner, name)` catalog row
  any more than `.signature` is: `"abc".^lookup("uc").package` is `(Cool)` (not `Str`);
  `Array.^lookup("push").package` and `Array.^find_method("elems").package` are both `(Any)` (not
  `Array`/`List`). This is the same option-(a)-vs-(b) tension `.signature` already showed, on a
  second axis — a hand-authored "true declaring type" table would need one entry per native
  method, independent of the arity/signature data.
- **`.candidates` is not just "vector of length > 1 when multi"**: even a non-multi method like
  `floor` answers `.candidates` with a *one*-element list containing itself
  (`Int.^lookup("floor").candidates.gist` → `(floor)`). A genuinely multi native method (built-in
  `Numeric` coercion, `Int.^lookup("Numeric")`) answers a 3-element `.candidates` list, and per
  earlier findings each element has `is_dispatcher=False`/`multi=True` while the dispatcher itself
  (the `.^lookup` return value) has `is_dispatcher=True`/`multi=False` — confirming #6420's fix
  shape (dispatcher vs. candidate-entry tags) is the right model, not specific to the cases #6420
  pinned.
- **A confirmed gap #6420 did not cover**: `Int.^lookup("Numeric")` (a real multi native method,
  unlike #6420's pins which were all non-multi or user-defined) still raises `No such method
  'is_dispatcher' for invocant of type 'Method'` on current `main`, instead of raku's `True`.
  #6420's fix is keyed off env tags (`__mutsu_lookup_*`/`__mutsu_is_multi_candidate`) set only at
  the specific call sites its own pins exercised; a native multi method's `.^lookup` result never
  gets those tags set, so it falls through to the still-open "no such method" error rather than the
  bogus `<composed-method:NAME>` the ticket's original repro showed. This reproduces the
  representation-mismatch root cause the ticket already describes (Sub-shaped vs. Method-Instance-
  shaped `.^lookup` result) on a case its own pin suite doesn't cover — not a new bug, but evidence
  the scoped patch's coverage is narrower than "any Method value answers `.is_dispatcher`/`.multi`
  correctly." Left unfixed here per the ticket's own guidance (best done as part of the unification,
  not another scoped patch) — repro is `Int.^lookup("Numeric").is_dispatcher`.

**Where this leaves the option (a) vs (b) decision:** two independent axes (`.signature`,
`.package`) both turn out to need hand-authored per-native-method data to match Rakudo exactly, not
just one. This makes option (a) (hand tables) more clearly the "faithful" choice and option (b)
(generic placeholder) more clearly the "cheap but visibly wrong" choice — but the *volume* of hand
data needed (roughly one declaring-type-and-signature-shape entry per native method name, likely
100+ entries across the 14 catalog owners) is exactly the scale of table F3 wants to retire
elsewhere in this same phase. This tension needs a user decision before a design doc is written;
see the ADR-0019 execution-plan entry for F1/F2's current status.

## Decision (2026-08-14): columns on the E2 catalog, not a second table

Consulted and confirmed with the user. Resolution, adopted as the plan going forward:

1. **Redefine what "hand table" means for this ADR's purposes.** The ban ANALYSIS §4-1 / this
   ADR's "Build an introspection-only catalog" rejected-alternative describes is on a *second
   source of truth about which methods exist and how they dispatch* — exactly what the 14
   `builtin_type_methods.rs` `&[&str]` arrays are, and exactly why F3 retires them without
   exception. `.package`/`.signature.gist` fidelity is a different category: **declaration
   metadata with no in-repo derivation** — Rakudo's own hand-written core signatures have no Raku
   source in mutsu to read them from, the same way `MethodDef::param_defs` is the only source for a
   *user* method's signature. This doesn't duplicate a canonical answer; for native methods, it
   *is* the canonical answer, expressed as data because Rust match arms can't carry it. F3's
   principle stays intact — no second `(owner, name)` name/existence list — while F1 is allowed to
   attach fidelity data to entries the *one* generated catalog already owns.
2. **No new parallel table.** Extend `NativeMethodRow` (`src/builtins/native_method_row.rs`) — the
   catalog E2 already generated once by probing real dispatch — with optional columns, e.g.
   `declared_package: Option<&'static str>` (`None` = fall back to the row's own `owner`) and a
   small `sig_shape` enum covering the handful of Rakudo signature-gist templates actually observed
   (generic named-catchall, raw capture, named-slurpy-with-real-name, ...). One key, one row: an
   entry introspection decorates is by construction an entry dispatch already resolves.
3. **Volume stays small by construction, not by discipline.** The raku ground truth above shows
   Rakudo's own native signatures are *mostly* the generic/arity-erased shapes already — a
   synthesized generic template (built from `NativeArityMask` + a default `sig_shape`) is close to
   correct for most rows without any override. Only the minority that diverge (`Hash.push`'s
   `+values`, `.package` corrections like `uc`'s `Cool` or `push`'s `Any`) get an explicit override
   entry, added lazily, one at a time, each backed by a raku-verified `t/` pin — not an upfront
   sweep of all ~350 native methods. A guard test ties every override to a live catalog row (fails
   loudly if a row is renamed/removed out from under its override), preventing the drift the
   rejected-alternative worried about.
4. **The dispatcher/multi/candidates axis needs no hand data at all.** It is a uniform structural
   rule (the `.^lookup` result is the dispatcher: `is_dispatcher=True`/`multi=False`; each element
   of its `.candidates` is a candidate: `is_dispatcher=False`/`multi=True`; a non-multi method's own
   `.candidates` is itself, one element) that generalized correctly across every case tested,
   *including* the native-multi case (`Int.^lookup("Numeric")`) #6420's narrower env-tag patch does
   not yet cover. F1's mechanism slice should implement this as general code in
   `make_native_method_object`/`classhow_lookup`, replacing the tag-matching patch, not add more
   tags.
5. **Sequencing** (keeps F1 and F3 independently landable, F3 first since it has no F1 dependency):
   - **F3** (next slice): cut `builtin_type_method_names()`'s three call sites
     (`methods_classhow_method_obj.rs`, `vm_call_helpers.rs`, `methods_classhow_builtin_methods.rs`)
     and `builtin_method_entries()`'s registry-seeding caller over to read from the canonical
     catalog/registry instead of the 14 arrays, then delete the arrays. No F1 dependency — this is
     pure "stop reading the old source, read the one already-generated source" plumbing.
   - **F1 mechanism slice** (no hand data): fix `make_native_method_object` per point 4, and merge
     in the `Method`-Instance-vs-`Sub` representation unification
     (`todo/tickets/classhow-lookup-returns-sub-not-method-instance.md`) so `.^lookup`'s result is
     the same shape `.^methods`/`.^can` already build. `.package` defaults to the row's `owner`;
     `.signature` defaults to a synthesized generic shape from arity. Pin the cases this already
     gets right without any override.
   - **F1 fidelity slice**: add the two override columns from point 2, populate only the surveyed
     mismatches above plus whatever a `t/`/roast assertion actually exercises, each raku-verified
     and pinned, with the guard test from point 3.
   - **ADR edit**: fold point 1's definitional boundary into the ADR itself (near F1's box text or
     the "Build an introspection-only catalog" rejected alternative), so a future agent doesn't
     misread F3 as blocking this, or misread this as license to resurrect name lists.
