# Hyper `».` reifies an explicitly `lazy`-marked finite Seq; `indir` stops over-marking its return

Fixed two related defects reported as #9838.

A hyper method call (`».`) over a finite `Seq` that was explicitly marked `lazy`
(`(lazy gather { take 1 })».succ`) silently answered `()` instead of reifying and
applying the method. The forcing guard in `exec_hyper_method_call_op`
(`src/vm/vm_hyper_method_ops.rs`) tested `.is-lazy` (`is_genuinely_lazy()`), which is
also `True` for such a list, and refused to force it — even though nothing about it is
actually infinite or unsafe to reify. Switched the guard to `LazyList::is_lazy_infinite()`,
the predicate that already distinguishes "genuinely infinite/unreifiable" from "merely
`lazy`-marked but finite", so only a truly infinite source is left unforced.

Separately, `indir` (and any other routine reached through `call_sub_value`) was marking
its own return value `.is-lazy` `True` even for a plain, unmarked `gather` — so
`indir "/tmp", { gather { take 3 } }` reported `.is-lazy` `True` (should be `False`) and,
combined with the first bug, `$h».succ` answered `()` instead of reifying. The cause was
`call_sub_value`'s return-value tail (`src/runtime/resolution_call_sub.rs`) unconditionally
rebuilding every returned `LazyList` with the `__mutsu_preserve_lazy_on_array_assign`
marker stamped on — the exact marker `.is-lazy` reads. A prior parity audit
(`news/2026-08/call-compiled-closure-rw-lazylist-gap-closed-no-live-bug.md`) had already
found this re-stamp redundant, since the marker lives on the `LazyList`'s own env and
travels with every clone of the `Value` regardless of call path; it just hadn't been
removed. Dropped the rebuild entirely.

Regression tests: `t/collections/lazy-seq/hyper-marked-lazy-gather.t`,
`t/io/indir-return-value-laziness.t`.
