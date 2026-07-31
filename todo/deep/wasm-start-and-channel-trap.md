# The WebAssembly build traps on `start` / `Channel` instead of degrading

Found 2026-07-23 building the tutorial site. Moved out of PLAN.md §8.18 when
discovered findings became per-file `todo/` entries.

## Root cause

`start { ... }` reaches `spawn_callable_promise` → `spawn_user_thread` →
`std::thread::spawn` (`src/runtime/builtins_system.rs:13`), which on
`wasm32-unknown-unknown` has no implementation and traps. In the browser that
surfaces as `RuntimeError: unreachable`, which also poisons the whole wasm
instance — every later evaluation in that session is garbage until the page
rebuilds the interpreter.

Affected: `start`, `Promise` combinators that spawn, `Channel` producers,
`Proc::Async`. `react`/`whenever` over a `Supply.from-list` already works (no
spawn), as does `gather`/`take`.

## Affected files

- `src/runtime/builtins_system.rs` — `spawn_user_thread`,
  `spawn_callable_promise`.
- The channel / supply pumps that spawn.
- `site/content/lessons.txt`, `site/e2e.test.mjs`.

## Why it is large

The likely correct fix is: on wasm, run a `start` block *synchronously* and
return an already-kept (or broken) `Promise`, and give `Channel` the same
treatment — a single-threaded scheduler is the honest semantics for a platform
with one thread, and it is what a reader of the concurrency chapter would expect
to see work. The mechanism is one `#[cfg(target_arch = "wasm32")]` arm in
`spawn_callable_promise` plus the equivalent for the channel/supply pumps, but
the *semantics* need thought: a `start` that blocks until it finishes changes the
observable ordering of any program that relies on interleaving, and `await` on a
never-kept promise would deadlock rather than trap.

## Acceptance test

The tutorial marks those two lessons `no-browser` in
`site/content/lessons.txt`, so the site explains the limitation and shows
the recorded native output rather than a trap. Removing those flags is the
acceptance test for this item; `site/e2e.test.mjs` sweeps every non-flagged
lesson in a real browser.
