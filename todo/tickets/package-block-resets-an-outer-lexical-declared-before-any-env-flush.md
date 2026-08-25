# A `module`/`package` block resets an outer `my` declared before any env flush

Found while fixing
`todo/tickets/in-file-package-our-var-clobbered-by-mainline-my.md`. It is a
*neighbouring* defect with a different root cause, it predates that work, and
it is unchanged by it (the repro below contains no `our` at all, so both of
that fix's gates are inert).

## Repro

```raku
my $x = "top";
module M { };
say $x;          # raku: top    mutsu: (Any)
```

`package P { }` fails identically. `class C { }` and a bare block `{ }` do
NOT — they take other scope-exit paths.

The trigger is not "first statement" as such but "no earlier statement has
materialised the lexical into `env`": inserting almost anything that flushes
the frame (`class Warm { }`, a `say`) before the `my` makes it pass, while a
plain `my $pad = 1;` before it does not.

```raku
class Warm { };
my $x = "top";
module M { };
say $x;          # raku: top    mutsu: top
```

## Root cause

`exec_package_scope_op` (`src/vm/vm_misc_reduction_scan.rs`) ends with

```rust
self.locals = saved_locals;
for (idx, local_name) in code.locals.iter().enumerate() {
    if let Some(val) = restored_env.get(local_name).cloned() {
        self.locals[idx] = val;
    }
}
```

The first line correctly restores the pre-block slot values. The loop then
exists so that a write the block body made to an *enclosing* lexical
(`my $x = 1; module M { $x = 2 }`) is not undone: such a write goes through
`env`, so re-reading `env` puts it back into the slot.

But the loop is unconditional, so it also fires for names the block never
touched — and there `env` can be *older* than the slot. mutsu's `locals`/`env`
dual store is lazy: `SetLocalDecl` writes `self.locals[idx]` and does not
always flush to `env`, which is only materialised later (a `Say` runs
`sync_env_from_locals_declared`, for instance). When `module M { }` is reached
before any such flush, `restored_env.get("x")` returns the stale pre-assignment
value and copies it over the live `'top'` in the slot.

Confirmed with `rust-gdb`: the refresh loop fires for `idx = 0`, `"x"` on the
empty-module repro, and the only `env` insert for the bare key `x` in the whole
program happens later, from `sync_env_from_locals_declared` under `OpCode::Say`
— i.e. after the block already clobbered the slot.

## Why it is not a one-liner

The loop needs to distinguish "the block wrote this name" from "this name has
a stale `env` entry", and the dual store carries no per-key dirty record that
would answer it. The candidate fixes each need care:

- Restrict the refresh to keys whose value the block actually *changed*
  (compare `current_env[k]` against `saved_env[k]`). Needs a cheap identity
  test on `Value`; a deep `eq` on containers is both wrong and expensive here.
- Track the keys re-imported from `current_env` into `restored_env` and refresh
  only those. Simple, but "the key existed before and after" is not the same as
  "the block wrote it", so the empty-module repro still refreshes.
- Flush the declaration eagerly so `env` is never stale. That is the §1.3
  dual-store debt and changes behaviour well beyond this site.

It is the same `locals`/`env` coherence family as
`project-slice-f-reverse-sync-campaign`, so it wants that context rather than a
local patch.

## Pin

`t/in-file-package-our-var.t` deliberately opens with a `class Warm { }` so its
mainline lexicals are materialised before the first package block; remove that
line to see this bug take out several of its assertions.
