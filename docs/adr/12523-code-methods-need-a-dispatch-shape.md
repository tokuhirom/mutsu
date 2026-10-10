# ADR-12523: `Code`'s methods need a dispatch shape

- **Status**: Proposed
- **Date**: 2026-10-10
- **Deciders**: tokuhirom, Claude
- **Issue**: [#12523](https://github.com/tokuhirom/mutsu/issues/12523)
- **Related**: [ADR-11276](11276-built-in-methods-are-handler-rows.md) (§9.52-§9.55 are the
  objects-group slices before this one), #12390 (the objects-group remainder)

## 1. Context

Rakudo declares `name`, `signature`, `arity`, `count`, `of`, `returns`, `line`, `file`, `Bool`,
`Str`, `gist`, `raku`, `Capture` and `clone` on `Code`. mutsu's recognition table names all of them
(`("Code", ...)` in `native_method_row_table.rs`), and `method-rows-report.py --inventory objects`
counts eleven as declared-but-unregistered (8 interpreter rows, 3 pure rows).

No row exists because a code object is not one value kind. These all answer to the owner `Code`:

| what `raku` calls it | mutsu value |
|---|---|
| `Sub`, `Block`, `Method` (a closure, a pointy block, a method object) | `ValueView::Sub(SubData)`; `WeakSub` is the same data behind a weak pointer |
| a `&name` reference to a multi, a builtin or a method | `ValueView::Routine { package, name, .. }`, resolved through the registry at call time |
| `Regex` (`/a/`, `token`, `rule`) | `ValueView::Regex`, or `Routine { is_regex: true }` |
| a `Sub` with a role mixed in (`Sub+{is-pure}`) | a mixin value around one of the above |

`DispatchShape` has no member for any of them, so a row cannot be reached by shape. The answers are
produced by `dispatch_sub_method` and `dispatch_routine_method` (`runtime/methods_sub.rs`, about
1500 lines of `if method == "..."` arms), `methods_regex_routine.rs` and `methods_instance_ops.rs`.
Several arms read registry state (`dispatcher_signature`, `has_multi_candidates`,
`callable_signature`), which is why the inventory counts them as interpreter rows.

The split also gives wrong answers. `Regex.of`, `Regex.returns` and `&say.of` throw
"No such method" where Rakudo answers `Mu` (#12523); each of those two kinds has its own arm list
(`methods_sub.rs` carries a `can` list of about two dozen names that has to agree with the arms by hand).

## 2. Decision (proposed)

1. **One closed `DispatchShape::Code`**, covering `Sub`, `WeakSub`, `Routine` handles and `Regex`
   values of the built-in classes `Sub`, `Block`, `Method`, `Regex`, `Routine`, `Code` (a user
   subclass of `Sub`/`Method`, or a mixin, has another class name and no shape, exactly as for
   `Date`). The handler decodes the kind; the rows are registered once, on the owner `Code`.
   Rejected alternative: one shape per value kind. The rows are the same for all of them and differ
   only in where the data lives, so per-kind shapes would repeat every row five times.
2. **Pure rows first.** `Bool`, `Str` and `Capture` read nothing but the value; `clone` copies the
   `SubData`. They move in the first PR together with the shape, because the shape is what makes
   them reachable.
3. **Interpreter rows read the registry through the `Handler::Interp` context**, never a new
   `Interpreter` field or `pending_*` slot: `name`, `signature`, `arity`, `count`, `of`, `returns`,
   `line`, `file`. The arms of `methods_sub.rs` they replace are deleted in the same PR, with the
   `// TODO: compile to bytecode` debt that goes with them.
4. **The `can` list in `methods_sub.rs` is deleted** once the rows exist; `.can` and `.^methods`
   read the recognition table like every other owner.
5. **User overrides keep winning.** The shape is closed to the built-in classes; a `method name`
   declared in a subclass of `Code`/`Sub` resolves before any row (the order the cascades already
   use).

## 3. Consequences

- `Regex.of`/`returns` and `&say.of` start answering `Mu` as a side effect of the rows existing.
- `methods_sub.rs` shrinks by the arms it hands to the rows; what remains (`wrap`, `unwrap`,
  `set_name`, `candidates`, `cando`, `assuming`) are not declared by Rakudo's `Code` and are tracked
  separately.
- One new `DispatchShape` member (38 of the 64 bits the call-site memo can hold); the audit for each
  `Any`/`Mu` row against the new shape (ADR-11276 §9.17) is part of the first PR.

## 4. Open questions

1. A `Routine` handle for a `multi` answers `arity`/`count` from all candidates, not one signature.
   Does the row keep that logic in the handler, or does the handle resolve to a single dispatcher
   `SubData` first?
2. `Sub+{is-pure}` (a mixin around a `Sub`): decode through the mixin in the shape, or leave it
   shapeless and reach the rows through its owner?
3. `gist`/`raku` on a `Sub` are `Interp` rows in the table (the rendering reads the signature); keep
   them in the first PR or split them off with the other rendering names?

## 5. Slice plan

1. Shape + pure rows (`Bool`, `Str`, `Capture`, `clone`) + the shape audit.
2. `name`, `line`, `file`, `signature` (single-value reads).
3. `arity`, `count`, `of`, `returns` (the multi-candidate logic), deleting the `can` list.
4. `gist`/`raku`, then flip the Status to Accepted with the Outcome section.

## 6. Amendment 2026-10-10: slice 1 needed no shape

The first slice (`of`, `returns`) shipped without `DispatchShape::Code`. `Signature`, `Exception` and `Failure` already show that a
family with no shape can register ordinary rows and be reached through its owner: `method_table::code::answer` calls
`invoke_owner(interp, &["Code"], ..)` from `dispatch_callable_method` (a `Sub`, `WeakSub` or `Routine` handle) and from the regex arm of
`methods_instance_ops.rs`. A shape is only needed if a `Code` row must be reached by the pure entries (the call-site lane, constant
folding), which no `Code` method wants: every one reads the registry or the closure's scope. Decision 1 above is therefore **withdrawn
for the interpreter rows** and stays open only for the pure rows (`Bool`, `Str`, `Capture`, `clone`). Slice 1 also fixed `Regex.of`,
`Regex.returns`, `&say.of` and `&infix:<+>.of`, which threw where Rakudo answers `Mu`. Slices 2-4 continue with `name`, `signature`,
`arity`, `count` (and `line`/`file`, whose regex, builtin and multi-dispatcher answers are still `Nil` where Rakudo knows a location).

Slice 2 (2026-10-10) moved `arity` and `count`. The two arms (a `&name` handle and a `Sub`) became `Interpreter::code_arity_count`,
which the `Code` rows call, so the regex introspection, the bound-signature, dispatcher and multi-candidate paths are one implementation.
Behaviour is unchanged (the pre-existing differences from Rakudo remain: `&say.count`, `&infix:<+>.arity` and a multi method's
`.arity`/`.count` from `.^lookup`).

Slice 3 (2026-10-10) moved `signature`. The `&name`-handle arm and the `Sub` arm became `Interpreter::code_signature` (a regex goes
through the same function), and the row calls it; behaviour is unchanged. Remaining differences from Rakudo are listed by the probe in
`t/routines/signature/routine-signature-rows.t`'s neighbours: a regex's `:(;; Mu |)` against `:(|)`, a multi method's and builtins'
signatures.

Slice 4 (2026-10-10) moved `name`, `line` and `file`. The two `line`/`file` arms (a `&name` handle and a `Sub`) became
`Interpreter::code_line_file`, and `name`'s `Routine`/`Sub`/regex branches in `methods_instance_ops.rs` became
`Interpreter::code_name_value`; the rows call them and the old `name` arm uses the same function for its `Code` receivers, keeping its
branches for the container descriptors and type objects (Rakudo declares those on other owners). Behaviour is unchanged. Left on
#12523: the pure rows (`Bool`, `Str`, `Capture`, `clone`) and `gist`/`raku`; the open questions of section 4 stand.
