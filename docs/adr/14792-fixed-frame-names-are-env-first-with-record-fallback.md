# ADR-14792: Fixed frame names are looked up env-first, with the frame record as the fallback

- **Status**: Superseded by [ADR-14843](14843-fixed-frame-names-live-in-the-frame-record.md) (was Proposed, 2026-10-23). Amended the fixture rule and the slice order of
  [ADR-14633](14633-call-frame-materializes-no-name-keyed-env.md) §2.1 and §4; every other
  decision of ADR-14633 stands.
- **Date**: 2026-10-23
- **Issue**: [#14792](https://github.com/tokuhirom/mutsu/issues/14792), a child of
  [#14633](https://github.com/tokuhirom/mutsu/issues/14633).

## 1. Context

ADR-14633 §2.1 decides that a lookup of a fixed name (`self`, `__ANON_STATE#`, `?CLASS`,
`?ROLE`) is answered by the innermost method frame's record *first*, then by the env chain, and
that debug builds assert the two agree. Implementing slices 1 and 2 found three facts that make
the record-first rule unusable until a much larger conversion has landed:

1. About 20 runtime helpers rebind the env `self` (or `?CLASS`) for the duration of a call
   (role mixin application, grammar actions, attribute-default evaluation, nested proto
   dispatch, regex code atoms, `with`-style rebinding in `attr_build_defaults.rs`, ...). While
   they run, the env is the truth and the record is the stale one.
2. A closure called inside a method keeps its creator's lexical `self`, and pushes no record of
   its own, so the innermost record belongs to the wrong method.
3. An `Env` lookup cannot see the interpreter's record stack, so the rule cannot live at the
   `Env` API as §2.1 describes; it can only live in `Interpreter`-level readers.

The debug assertion fired in `t/oo/self-is-lexical-in-blocks.t`,
`custom-how-explicit-self-and-role-default.t` and `proxy-subclass-*` for exactly these reasons.

## 2. Decision

**A lookup of a fixed name reads the env key first and falls back to the innermost method
record.** A frame omits its fixed keys only when none of the four names is visible in its env
chain (one membership probe per name at frame entry; no analysis of the body or of other code).

- A reader that finds an env key is answered by the frame that wrote it, so every helper that
  rebinds a name keeps working without being converted, and a closure's installed lexical `self`
  wins over the record of whichever method is running.
- A frame under a caller that still writes its keys writes its own, exactly as before. Because
  of that, an env-first reader never finds another frame's stale key where this frame omitted
  its own: omission requires that there be no key to find.
- The record answers only where the env has no key: a frame that omitted its keys, and the
  helpers called from it.
- A closure created in a frame that omitted its keys takes the ones it needs into its capture
  at creation (`self` already is taken from the frame's slot; `__ANON_STATE#`, `?CLASS` and
  `?ROLE` are taken from the record), so a later call of it sees its creator's names, not the
  record of the method it runs under.
- Readers go through `Interpreter::env_or_fixture_sym` (and `env_self_value`) and the by-name
  `GetGlobal` path falls back the same way. Code that only saves and restores an env key
  stays env-only, so it never turns a record into an env key.

The debug equality assertion of ADR-14633 §2.1 is dropped: env and record legitimately differ
while a helper has rebound a name.

### Slice order

ADR-14633 §4 slice 3 (convert readers to accessors and delete the fixture rule) is deferred. With
env-first the rule is the end state for as long as the rebinding helpers exist; deleting it
requires converting each of them to a scoped record helper and giving closure frames records of
their own, which is a separate decision to take when the frames that still write keys (methods
that cannot omit them) are measured to matter. Slices 4 (lazy frame env) and 5
(`FrameSideState`) apply to the frames that already omit their keys and carry the measurable
win; they do not wait for slice 3.

## 3. Consequences

- Soundness needs no analysis: the chain probe at entry decides whether to write, and a reader
  that finds nothing is answered by the record, which is correct because the nearest frame that
  wrote a key would have been found first.
- A frame whose chain shows a key still pays the old four inserts. This is the case for every
  callee of a frame that wrote keys (methods with closures, the slow call path, class bodies),
  so the saving applies to leaf-like call chains first.
- A record that is read after its frame returned is still impossible: readers read the top of
  the stack.

## 4. Alternatives rejected

- **Record first (ADR-14633 §2.1 as written).** Needs every rebinding helper and every closure
  call converted before any key can be omitted; wrong in the meantime (the assertion failures
  above).
- **Newest-wins by comparing depths.** An env key carries no frame depth; adding one costs
  a word per insert on a hot path to avoid a probe per call.
