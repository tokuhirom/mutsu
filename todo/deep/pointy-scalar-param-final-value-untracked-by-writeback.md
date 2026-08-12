# `given`/`with EXPR -> $v is rw { ... }` never writes back — the pointy param's mutated value is untracked by BOTH locals and env at block exit

Supersedes `todo/tickets/given-with-explicit-rw-pointy-param-element-topic-no-writeback.md`
(same symptom, narrower diagnosis) — this file replaces it with the deeper root
cause found while attempting the fix. The bug turned out to be broader than
"element topic + `is rw`": it affects **any scalar pointy parameter** bound
through a `given`/`with` topic (element source or whole-variable source
alike), `is rw` or not.

## Symptom

```raku
my %h = a => 1, b => 2;
given %h<a> -> $v is rw { $v += 10 }
say %h<a>;   # raku: 11 — mutsu: 1 (unchanged)

my $x = 1;
given $x -> $v is rw { $v += 10 }
say $x;      # raku: 11 — mutsu: 1 (unchanged)

my %h2 = a => 1;
given %h2<a> -> $v is rw { $v = 99 }   # full reassign, not +=
say %h2<a>;  # raku: 99 — mutsu: 1 (unchanged)
```

A non-pointy topic already writes back correctly (`given %h<a> { $_ += 10 }`,
`given $x { $_ = 99 }` both work — pinned by `t/given-element-topic.t`), and a
pointy **aggregate** (`@`/`%`) parameter writes back correctly too (`given @a
-> @p { @p.push(...) }`). Only a **scalar** pointy parameter (`-> $v`, with or
without `is rw`) is broken. (`-> $v` without `is rw` should additionally DIE
with `Cannot assign to a readonly variable` on real Raku, which mutsu also
gets wrong — it silently allows the assignment and then loses it the same
way. That readonly-enforcement gap is a second, related bug in the same area,
not investigated further here.)

## Where the writeback mechanism lives

`exec_given_op` (`src/vm/vm_given_when_ops.rs`) computes a `pointy_param:
Option<String>` from the compiler-detected bound name (`pointy_param_name` in
`src/compiler/stmt.rs` around line 2848, matching the parser's
`pointy_topic_bind` synthetic `MarkBind` + `VarDecl` output —
`src/parser/stmt/control.rs:113`). At block exit it calls either
`write_back_element_source` or `write_back_given_topic`
(`src/vm/vm_loop_writeback.rs`), both of which read the pointy param's
**final value** via:

```rust
let current = match pointy_param {
    Some(p) => self.get_env_with_main_alias(p),
    None => self.env().get("_").cloned(),
};
```

`get_env_with_main_alias` reads **only `env()`** — no locals lookup. This
looked at first like the well-known dual-store gap (`gate_local_slot_value`,
`src/vm/vm_env_helpers.rs:1722`, exists for exactly this class of "the
mutation only landed in the local slot" bug elsewhere in the codebase).

## What was tried and why it didn't work

Added `self.gate_local_slot_value(code, p).or_else(|| self.get_env_with_main_alias(p))`
to both writeback functions (mirroring the established `(B)` per-store
env-write gate pattern). **Did not fix it** — reverted (do not re-apply this
exact patch without first resolving the finding below; the diff is
reconstructable from this description if a future attempt wants a starting
point, but it's the wrong layer of the problem).

## What's actually going on (confirmed via `rust-gdb` + a temporary
`MUTSU_DEBUG_WRITEBACK` eprintln, both removed before this file was written)

For `given %h<a> -> $v is rw { $v += 10 }`, at the writeback call site:

- `pointy_param = Some("v")` — correctly detected.
- `code.locals = ["%h", "v"]`, `code.plain_locals = [false, true]` — slot 1 is
  registered for `"v"` and IS marked a plain scalar lexical at compile time.
- `gate_local_slot(code, "v")` resolves to `Some(1)` (slot found, plain flag
  set) — but `self.locals[1]` reads back **Nil**.
- `self.env().get("v")` is **also `None`** — not just stale, genuinely absent.

So `$v`'s value is tracked in **neither** store at the point the writeback
runs — yet a bare `say $v` *inside* the same block correctly prints `1` (the
bound value) when the body doesn't mutate it
(`tmp/rw-read-only-check.raku`-shaped repro). That means the ordinary
variable-read path (`Expr::Var("v")`, whatever opcode that compiles to) finds
the value through some THIRD channel that neither `gate_local_slot_value` nor
`get_env_with_main_alias` consult.

The likely suspect, not yet confirmed: the `__mutsu_bound_decont::<name>`
marker mechanism (`update_bound_decont_marker` /
`vm_var_assign_coerce.rs:551-595`, consulted by the plain variable-read opcode
at `vm_exec_dispatch.rs:1918-1928`, and explicitly cleaned up for a pointy
param at `vm_given_when_ops.rs:149`: `remove(&format!("__mutsu_bound_decont::{}", p))`).
This exists specifically for `:=`-bound scalars (`pointy_topic_bind`'s general
case emits exactly the `MarkBind` + `VarDecl` shape this marker is designed
for) and is a **third store** alongside plain locals and env that neither
`gate_local_slot_value` nor `get_env_with_main_alias` know about. If this is
where `$v`'s live value actually lives, the writeback functions need to read
`env().get(&format!("__mutsu_bound_decont::{}", p))` (or whatever it actually
holds — not yet inspected) instead of / in addition to the two stores they
already check.

**This was not confirmed** — the investigation stopped here for this session
rather than opening a fourth debugging pass on a mechanism (`bound_decont`)
that was not designed for this codepath and whose exact value shape is
unknown. Whoever picks this up next should:

1. Break at `vm_exec_dispatch.rs:1918` (`is_bound_decont` check) while running
   the `say $v` read-only repro to see what the read path ACTUALLY resolves
   `v` from, then compare against what `write_back_element_source` sees at
   block exit for the mutating repro.
2. If `__mutsu_bound_decont::v` is confirmed as the live store, decide whether
   the fix is (a) teach the writeback functions to also check it, or (b)
   whether `$v += 10`'s *write* path should be updating `plain_locals[1]`/`env["v"]`
   in the first place and doesn't (i.e., the bug is on the write side, not the
   read side of the writeback) — these are different fixes and the repro above
   doesn't yet distinguish them.
3. Only then decide whether a `gate_local_slot_value`-style helper fix
   (reverted from this session) becomes correct once the right store is
   identified, or whether the real fix is elsewhere (e.g. in how
   `pointy_topic_bind`'s `MarkBind` + `VarDecl` shape is compiled/executed for
   a scalar specifically).

## Reproduce

No fixtures/modules needed — `tmp/with-rw-elem-check.raku`,
`tmp/pointy-scalar-writeback-check.raku`, `tmp/rw-read-only-check.raku` (all
gitignored scratch, recreate from the snippets above if they're gone).
