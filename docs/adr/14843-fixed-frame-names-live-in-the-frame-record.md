# ADR-14843: `self`, `::?CLASS` and `::?ROLE` live in the frame record only

- **Status**: Accepted (user decision 2026-10-31; proposed 2026-10-29). Supersedes
  [ADR-14792](14792-fixed-frame-names-are-env-first-with-record-fallback.md), and restores the
  record-first rule of [ADR-14633](14633-call-frame-materializes-no-name-keyed-env.md) §2.1 with
  the converted helpers and closure records ADR-14792 found missing; every other decision of
  ADR-14633 stands.
- **Date**: 2026-10-29
- **Issue**: [#14843](https://github.com/tokuhirom/mutsu/issues/14843), a child of
  [#14633](https://github.com/tokuhirom/mutsu/issues/14633).

## 1. Context

ADR-14792 made the lookup of a fixed name env-first with the frame record as a fallback, because
three things were true: about 20 helpers rebound the env `self`/`?CLASS` around a call, a closure
called inside a method pushed no record of its own, and an `Env` lookup cannot see the
interpreter's record stack. It called the rule transitional: "deleting it requires converting each
of the helpers to a scoped record helper and giving closure frames records of their own".

Both conversions are now done, so the env-first rule has nothing left to protect.

## 2. Decision

**A method frame's invocant (`self`), `::?CLASS` and `::?ROLE` are stored in its
`FrameFixtures` record and nowhere else. No `Env` key holds them.**

1. *Closures carry their creator's record.* `SubData` and `LazyList` have a `fixtures` field,
   filled from the creating frame (`capture_fixtures`). A call of a closure, gather body, routine
   value or inline map/grep callback pushes its frame and continues that record
   (`attach_closure_fixtures`). A closure called under a different method, or after its creator
   returned, answers its creator's names; it never reads an unrelated caller's record.
2. *Rebinding is a scoped record.* The helpers that rebound `self`/`?CLASS` for a call open a
   scope with `begin_scoped_invocant` / `begin_class_scope` / `push_scoped_fixtures` and close it
   with `end_scoped_fixtures`. `RoutineStack::pop` and `truncate` cut the records with their
   frames, and a scope is cut back on every exit of its region, so nesting restores the outer
   override on return, on an exception and on a non-local exit. A CONTROL or CATCH handler run
   inline at a raise site pushes the record its installing frame had when the handler was
   registered.
3. *Method entry writes no key.* The fast path and the slow path push the record with the routine
   frame (`push_method_routine_with_location`) and write neither `self`, `?CLASS` nor `?ROLE`.
   The parameter binder writes no env key for a method's implicit invocant
   (`ParamDef::is_implicit_invocant`). A method run as a function (a method literal, a `my
   method` called as a sub, a Proxy `FETCH`) gets its invocant after its frame is pushed
   (`adopt_function_invocant`, `install_function_invocant`), or, for a defaulted invocant
   (`method ($c = 42:)`), from the default the binder evaluated.
4. *Readers ask the record.* `frame_invocant`, `frame_fixture_value` and the by-name chokepoint
   (`get_env_with_main_alias_sym`) answer `self`, `?CLASS` and `?ROLE` from the record. A closure
   invoked inline (`call_compiled_closure_in_unit`) seeds a local named for one of the three from
   the record.
5. *A by-name store of `self` reaches the record* (`store_invocant_by_name`,
   `set_env_with_main_alias_inner`, the store places of `StorePlace::Named`). `self` is never a
   named subscript target in the compiler (`postfix_index_name`): `self{$k}:delete` and the like
   work on the invocant expression.
6. *`__ANON_STATE#` is not a fixed name.* It is the spelling of the bare `$` variable
   (`t/lang/parsing/bare-sigil.t`) and an ordinary lexical; the fixed names are the three above.
7. *The audit is a ratchet.* `make check-frame-fixtures` counts the references to the three names
   in the runtime layers per file (one baseline file per source file under
   `scripts/frame-fixtures.d/`, [#15467](https://github.com/tokuhirom/mutsu/pull/15467)); a count
   may only rise by an edit a reviewer sees, and a drop needs no baseline edit. It is an inventory
   of name tests, not proof that no `Env` key is ever built from a computed name; with the scoped
   helpers as the only way to rebind a name for a region, it is the auditable ratchet the
   completion criterion of #14843 asks for.

## 2.1 A plain routine's `$self` parameter is not the `self` term

A parameter spelled `$self` is a user lexical (ADR-0061). In every callable, method or plain
routine, pointy block, `for`/`map` block or `add_method`ed sub, it binds the reserved `LEX_SELF`
(`$self`) key (`ParamDef::storage_name`); the `self` term is read only from the frame record. The
argument bound to a `$self` parameter is never installed as the frame's invocant, except for an
explicit `$self:` invocant of a method (`function_invocant_arg`). So `method both { my $f = sub
($self) { $self ~ self.^name }; $f("x") }` prints `xD`, as raku does
([#15464](https://github.com/tokuhirom/mutsu/issues/15464), #15476, #15477;
`t/oo/self-param-plain-routine-lexical.t`, `t/oo/self-param-lexical.t`).

## 3. Consequences

- A call frame has no fixed-name inserts: the `Env` of a method call is smaller and the four
  inserts per call are gone on the slow path.
- A reader has one place to look. The debug equality assertion of ADR-14633 §2.1 has nothing to
  compare and stays dropped.
- The scoped helpers are the only way to make `self` mean something else for a region; a new
  helper that writes the env key is caught by `check-frame-fixtures`.

## 4. Alternatives rejected

- **Keep env-first (ADR-14792).** Leaves two sources of truth and a probe per frame entry to decide
  whether the keys may be omitted.
- **Record first with an env fallback for old helpers.** The helpers were converted instead;
  keeping the fallback would keep the stale-key class of bug alive.
- **A frame-depth tag on env keys.** Costs a word per insert on a hot path to avoid a problem the
  record does not have.
