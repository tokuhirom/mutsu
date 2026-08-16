# A failed class redeclaration's rollback does not restore a `proto method` it declared

`ClassRegSnapshot::restore` (`src/runtime/registration_class_validate.rs`) rolls back a class
redefinition that failed partway through (a body statement returned `Err`) by restoring the
registry's user-method rows, attributes, composed-role bookkeeping, etc. from the pre-attempt
snapshot. This is a pre-existing gap, not something ADR-0019 F4c-8 introduced or is expected to
fix — recorded per that box's design note (4)(a), which explicitly says to file this rather than
fold a behavior change into F4c.

**The gap:** `MethodEntry::proto` (the `proto method`/`proto submethod` column added in ADR-0019
E8/E8b/E8c) is not part of either the old world's rollback (`registry.rs`'s `sync_user_method_
entries` retain step deliberately spares the `proto` column so a repair-sync doesn't wipe it) or
F4c-8's own `prev_method_rows`/`restore_user_method_rows` mechanism (`restore_user_method_rows`
only ever touches `user_candidates`, by design — see `Registry::set_user_methods`'s row-liveness
predicate). So: a class redeclaration that (1) declares a `proto method foo {*}` and (2) later in
the same body fails and rolls back, leaves the `proto` row for `foo` installed on the class even
though every other side effect of the failed attempt was undone.

**Why this survives unnoticed today:** a `proto` method with no candidates is essentially inert
until a same-named regular method is also declared — dispatch through a proto with zero candidates
either falls through or errors depending on call site, so the dangling proto rarely produces a
directly-visible symptom. It would only manifest if the SAME class name is redeclared again later
in the same program and the new declaration expects a clean slate for that method name, or if
introspection (`.^can`, `.^find_method`) surfaces the stale proto.

**Repro sketch (not yet verified/reduced):**
```raku
class Foo {
    proto method bar {*}
    method missing-thing { CALLS-SOMETHING-UNDECLARED() }  # or any statement that dies
}
# `class Foo {...}` fails and (depending on how the failure surfaces -- EVAL,
# try/CATCH around the whole declaration, etc.) the registration rolls back.
# Foo.^find_method('bar') may still report a (candidate-less) proto.
```

**Fix sketch:** either (a) add a `prev_proto: Option<FunctionDef>` (or a small per-name map) to
`ClassRegSnapshot`, captured via a new `Registry::method_entry_proto`-based read and restored via
a new mutator that writes just the `proto` column, mirroring `prev_method_rows`'s shape; or (b)
decide this specific gap is acceptable permanently and downgrade this ticket to documentation only.
Either way, resolve with a real `raku` behavior check first — Rakudo's own semantics for what a
failed-and-rolled-back class redeclaration leaves behind is the ground truth, not assumption.
