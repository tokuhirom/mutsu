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
