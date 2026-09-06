# `Metamodel::Naming` / `Stashing` / `Primitives` are not composable types

> Renamed from `direct-metamodel-classhow-new-type-immutable-error.md` on
> 2026-09-07. That file has now had **three** framings closed under it, each one
> measured to be wrong or already fixed before the next was written; what is
> left is the one thing it recorded as an aside. The history is kept below
> because the pattern is the point.

## The three closed framings

1. **"`constant A := Metamodel::ClassHOW.new_type(...)` dies `Cannot modify an
   immutable 'A' type object`."** Fixed 2026-08-26 — the guard in
   `src/vm/vm_exec_dispatch.rs` was over-broad and now exempts a `constant`
   *declaration*, which binds a name rather than modifying what the name means
   (`news/2026-08/metamodel-how-set-why-after-compose-immutable.md`).
2. **"A method-dispatch gap on a `new_type`-minted Package: `.^add_method` +
   `.^compose` record the method, but calling it is a silent no-op."** Wrong
   about `new_type`: an ordinary `class C { }` showed it identically. The real
   cause was `.^add_method` installing an **empty** body for any NAMED,
   separately-declared routine, because such a Sub carries its bytecode in
   `SubData::compiled_routine` rather than `compiled_code` (ADR-0019 C6c keeps
   the two apart — different calling conventions). Fixed 2026-09-06,
   `news/2026-09/add-method-installs-a-routine-backed-code-object.md`.
3. **The doc's full worked example** (`Language/mop.rakudoc:34`) now agrees with
   raku end to end, re-measured 2026-09-07:

   ```raku
   constant A := Metamodel::ClassHOW.new_type(name => 'A');
   A.^add_method('x', my method x(A:) { say 42 });
   A.^compose;
   say A.^methods.elems;      # 1        both
   say A.^lookup('x').^name;  # Method   both
   A.x();                     # 42       both
   ```

## What is still open

```raku
class WithStashHOW
    does Metamodel::Naming
    does Metamodel::Stashing
{
    method new_type(WithStashHOW:_: Str:D :$name! --> Mu) {
        my WithStashHOW:D $meta := self.new;
        my Mu             $type := Metamodel::Primitives.create_type: $meta, 'Uninstantiable';
        $meta.set_name: $type, $name;
        self.add_stash: $type
    }
}
my Mu constant WithStash = WithStashHOW.new_type: :name<WithStash>;
say WithStash.WHO;   # raku: WithStash
```

mutsu dies `X::InvalidType: Invalid typename 'Metamodel::Naming'` — the `does`
clause fails before the class body is considered, because `Metamodel::Naming`,
`Metamodel::Stashing` and `Metamodel::Primitives` are not registered as
composable roles/types at all.

This is the "script type creation directly through `Metamodel::*`" question,
and it is genuinely larger than anything above it: it needs those to exist as
real roles users can `does` and call, not as names the `Metamodel::` prefix
match happens to accept. `Type/Metamodel/Stashing.rakudoc:45` is the worked
example. Adjacent and probably the same slice:
`src/runtime/methods_instance_ops.rs` (~line 2138) dispatches `.new_type`
generically for *any* `Metamodel::*` package name, always registering a plain
empty class and returning a bare `Package` — so the resulting type object's
`.HOW` always introspects as `ClassHOW`-shaped regardless of which metaclass
was invoked. That half has its own minimal repro in
`todo/tickets/metamodel-parametricrolehow-new-type-wrong-how.md`.

## Two smaller residues, deliberately deferred (measured 2026-09-07)

Both were found beside framing 2 and are recorded so they are not re-discovered:

- **The installed method reports the name it was ADDED under**, where raku
  reports the routine's own name: for
  `A9.^add_method('m', my method m9b() { 9 })`, `A9.^lookup('m').name` is `m` in
  mutsu and `m9b` in raku. Deferred because `MethodDef` has no name field and
  gaining one touches its 18 construction sites, for a purely introspective
  difference with no corpus consumer.
- **A plain `sub` used as a method does not receive the invocant as its first
  positional.** raku installs the Sub as-is and prepends the invocant to the
  argument list, so `sub plain($x)` added as `p` makes `B.p(21)` die "Too many
  positionals passed; expected 1 argument but got 2", while
  `sub plain2($inv, $x)` answers `63`. mutsu has it exactly inverted: `plain`
  answers `42` and `plain2` dies "Too few positionals passed". Deferred on
  corpus evidence: every `.^add_method` call in `roast/`, `vendor/` and
  `modules/` passes a **method**, never a plain sub
  (`roast/6.c/MISC/bug-coverage-stress.t`, `roast/S12-meta/grammarhow.t`,
  `roast/S12-introspection/meta-class.t`, `roast/S12-class/basic.t`,
  `roast/S14-roles/composition.t`, `modules/OO-Monitors`, `modules/Text-CSV`,
  `modules/NativeHelpers-Blob`), so there is nothing to unblock. Closing it
  needs an `invocant_is_first_positional` bit on `MethodDef` honoured at all
  four method-dispatch entry points.

## Affected files

- Wherever `Metamodel::*` roles would have to be registered as real composable
  types — no such site exists yet; `src/runtime/methods_instance_ops.rs`
  (~2138) is where the generic `Metamodel::*` prefix match lives today
- `src/runtime/methods_classhow_dispatch.rs` — the `"add_method"` arm, for the
  two deferred residues
- `src/runtime/decl_types.rs` — `MethodDef`, which both residues would extend
