# Recursive sub storing a trailing-comma list literal of its own parameter into a `my @` local crashes with a Rust stack overflow

Discovered while resolving
`todo/tickets/bind-alias-saved-locals-wrong-frame-index.md` (now
`news/2026-08/bind-alias-saved-locals-dead-code-removed.md`), while building
a repro for a *different*, narrower finding
(`todo/deep/bind-propagate-ancestor-frames-clobbers-unrelated-recursive-locals.md`).
This bug turned out to have nothing to do with `:=` bind or block scoping —
both were red herrings from the original repro shape. It is a general,
severe (process-crashing) bug in ordinary recursion, unrelated to this
session's actual ticket, so it is filed separately.

## Minimal repro

```raku
sub rec(Int $n) {
    my @v = ($n,);
    if $n > 0 {
        rec($n - 1);
    }
}
rec(1);
say "done";
```

- `raku`: prints `done`.
- `mutsu` (current `main`, confirmed on a clean, non-worktree-modified
  checkout): `thread 'mutsu-main' (...) has overflowed its stack / fatal
  runtime error: stack overflow, aborting` (SIGABRT, exit 134). Reproduces
  even at recursion depth 1 (`rec(1)`), i.e. exactly one recursive call.

## What narrows it down (all verified on current `main`)

- `my @v = ($n,);` (parenthesized list literal, **trailing comma**,
  containing the routine's own parameter `$n`) — crashes.
- `my @v = ($n);` (same, but no trailing comma — just a parenthesized
  scalar, not a 1-element List) — does NOT crash.
- `my @v = (1,);` (trailing comma, but a literal constant instead of the
  parameter) — does NOT crash.
- `my @v = [$n];` (square-bracket array literal instead of a parenthesized
  list) — does NOT crash.
- The same `my @v = ($n,);` line in a **non-recursive** sub — does NOT
  crash (prints `[1]` correctly).
- `rec(0)` (parameter such that the recursive branch is never actually
  taken) — does NOT crash.
- Whether the sub/array live at the file's mainline scope or inside a bare
  `{ ... }` block makes no difference — both crash.

So the crash needs ALL of: (a) the sub actually recurses (calls itself) at
least once at runtme, (b) the local array is built from a parenthesized
list literal with an explicit trailing comma (the syntax that forces
list/Array context for a single element, as opposed to plain scalar
parenthesization), and (c) that literal's single element is the routine's
own parameter (a lexical bound fresh on every call), not a constant.

## Suspected area (not confirmed by a debugger session — next step for whoever picks this up)

The requirement for (b)+(c) together (a fresh per-call array built from the
call's own parameter) plus the crash shape (unbounded *native* Rust
recursion, not a raku-level infinite loop — `rec(1)` only recurses once at
the Raku level) points at one of the array-construction/COW/circular-
reference-fixup helpers being unexpectedly invoked and unexpectedly
recursing without a base case, most likely one of:

- `Interpreter::fixup_circular_array_refs` / `value_contains_array_ref` /
  `replace_array_refs_in_value` (`src/vm/vm_var_assign_ops.rs`) — these
  walk a freshly-assigned array's elements looking for (and rewriting) a
  self-reference to the array's own old backing `Gc`, recursively, with no
  visible depth bound beyond `seen_hashes` (which only guards *hash* re-entry,
  not general recursion depth).
- `Interpreter::array_inplace_reassign` (same file), which calls
  `replace_array_refs_in_value` on every element of a freshly-cloned array
  during a whole-container-identity-preserving store.
- Something in how a trailing-comma single-element list literal is
  compiled/constructed differently from `[$n]` or `($n)`, that ends up
  aliasing the new array with a stale `Gc` pointer from the CALLER's own
  frame (a lexical named `$n`/`@v` reused verbatim across recursive
  invocations, similar in spirit to the same-name-across-recursion issue in
  the sibling `todo/deep` file above) — which would explain why *only* the
  recursive+parameter combination triggers it: the fixup functions are
  designed to detect exactly this "does this array contain a stale
  reference to itself" shape, and something about the trailing-comma
  construction path might be handing them a genuinely (accidentally)
  self-referential structure that isn't actually self-referential, so the
  traversal never terminates on the same object it just cloned.

## Suggested next step

Use `rust-gdb -batch` (per `CLAUDE.md`'s debugging guidance) on the minimal
repro above to catch the actual stack-overflowing recursive function via a
backtrace at the point just before the overflow (e.g. break on `panic!`/
`abort`, or use `ulimit -s` to shrink the stack so it overflows faster and
a `bt` shows the repeating frame). That will confirm or rule out the
suspects above in one session, without guessing.
