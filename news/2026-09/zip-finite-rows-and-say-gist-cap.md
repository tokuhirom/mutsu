# `zip` keeps every finite row; `say @a` prints the capped gist

Two correctness bugs from the Array complexity audit (issue #9160).

**`zip` truncated finite input to 1000 rows.** `zip(^5000, ^5000).elems` was
`1000` (rakudo: `5000`), and `zip(..., :with(&[+]))` did the same. Both the
plain `zip` (`functions/dispatch_variadic.rs`) and `builtin_zip_with`
(`runtime/builtins_reduce.rs`) applied a 1000-row cap unconditionally. The
cap exists to bound how much of an infinite column gets materialized, so it
now applies only when at least one column is lazy. A zip that mixes a finite
column with an infinite one is still bounded by that prefix; making `zip`
genuinely lazy is the general fix and is left to the laziness work.

**`say @a` / `note @a` rendered every element.** `@a.gist` and `@a.say` stop
after 100 elements and append `...`, but the function forms render through
`gist_value` (`runtime/utils/gist.rs`), whose Array, Seq and Slip arms joined
every element. They now share one `gist_elements` helper that stops at the
same 100-element cap, which also caps nested aggregates (`say [[^200],]`) the
way rakudo does.

Pinned by `t/collections/transform/zip-finite-rows-and-say-cap.t`.
