# A native-typed `given`/`with` pointy param with `is rw` does not write back

## Symptom

```raku
my int $x = 1;
given $x -> int $v is rw { $v = 99 }
say $x;
```

raku prints `99` (the `is rw` alias writes back to `$x`, matching the
non-native scalar pointy-param case). mutsu prints `1` — the mutation to `$v`
never reaches `$x`.

Confirmed with `git stash` against a clean `main` checkout (2026-08-12) that
this is pre-existing and unrelated to the readonly-enforcement fix landed
alongside this ticket (`given-with-pointy-scalar-missing-readonly-enforcement.md`)
— `main` already printed `1` here before that fix.

## Where this likely lives

`pointy_topic_bind` (`src/parser/stmt/control.rs`) has a dedicated branch for
a native-typed pointy param (`pd.type_constraint` starting with a lowercase
ASCII letter, e.g. `int`/`num`/`str`) that compiles to a plain `Stmt::VarDecl`
binding by VALUE (`expr: topic`), not `:=` (unlike the general `$`/`@`/`%`
branch just below it, which uses `MarkBind` + `VarDecl` so the compiler
treats it as an aliasing bind). The doc comment on that branch says
"Native-typed lexicals cannot participate in `:=` binding" — true for the
*bind mechanics*, but the branch does not do anything else to arrange
writeback for the `is rw` case either (no equivalent of `exec_given_op`'s
`pointy_capture_slot`/writeback machinery being told to treat this slot as
rw-aliased).

## Suggested next steps (not investigated further)

1. Check whether `exec_given_op` (`src/vm/vm_given_when_ops.rs`) even
   attempts a writeback for a native pointy param — the `pointy_capture_slot`
   detection there scans for the first `SetLocalDecl` in the body, so it
   likely fires for the native branch too (same op is emitted), but the
   writeback might restore the wrong thing, or the native VarDecl might not
   round-trip through the same "unboxed native value in its own container"
   path that `write_back_given_topic` expects.
2. Compare against how native-typed ordinary sub/block parameters with `is
   rw` write back (if they do at all — natives are unboxed values, so a
   native `rw` parameter may need a different mechanism entirely, e.g.
   binding to the source's underlying container rather than aliasing a
   value).
3. Write a minimal repro without `given`/`with` first (a plain native pointy
   block param outside a topicalizer, if that syntax is even valid in raku)
   to isolate whether this is a `given`/`with`-specific gap or a more general
   native-parameter-aliasing gap.

## Reproduce

`my int $x = 1; given $x -> int $v is rw { $v = 99 }; say $x;` — no
fixtures needed. Expected (raku): `99`. Actual (mutsu): `1`.
