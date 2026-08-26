# A `gather` Seq is not lazy, so hyper / `.map` / `.grep` over it stop answering `()`

From the doc-diff harness (`Type/independent-routines.rakudoc:312`, the `indir`
example). Reduced, with no `indir` involved:

```raku
my $g = gather { take 1; take 2 };
say $g>>.Str;        # raku: (1 2)   mutsu: ()
say $g.map(* + 1);   # raku: (2 3)   mutsu: (...)
```

## Root cause — two independent gaps, both about "is this lazy?"

**1. Hyper read an unforced cache.** `exec_hyper_method_call_op` collects its
elements through `hyper_source_items` → `value_to_list`, a *static* reader that
cannot run the VM. For a `LazyList` it returns the cache, which for a
never-forced `gather` coroutine is empty — so the hyper looped over zero
elements and answered `()` with no error.

**2. `.is-lazy` was re-derived, wrongly, in three places.** mutsu already had a
single authority, `LazyList::is_genuinely_lazy`, whose doc comment states the
correct rule ("a plain `gather` is `.is-lazy` `False` in Rakudo"). But:

- its `lazy_pipe` arm answered `true` for **every** `.map`/`.grep` pipe that was
  not a `SkipFirst` index transform, regardless of the source — so
  `gather {...}.map(*+1)` rendered the `(...)` placeholder instead of its
  elements;
- the `.is-lazy` *method* (`dispatch_core_str.rs`, and its twin in
  `methods_dispatch_match3.rs`) ignored `is_genuinely_lazy` entirely and
  answered `!has_finite_closure_endpoint && !is_cat_pull` — i.e. `True` for a
  plain `gather`;
- `.elems` had a third variant, gated on the `__mutsu_lazylist_from_gather` env
  marker, which a *pipe over* a gather does not carry, so `$g.map(*+1).elems`
  threw `X::Cannot::Lazy`.

## Fix

`is_genuinely_lazy`'s pipe arm now delegates to the existing conservative
`pipe_bottoms_out_finite()` (a pipe is lazy exactly when its source chain cannot
be proven finite; an unrecognized source keeps the old lazy answer, so this can
never turn an infinite pipe into a hang). Both `.is-lazy` sites and `.elems` now
call `is_genuinely_lazy` instead of re-deriving it, and the VM's
`X::Cannot::Lazy` guard for a strict force exempts a pipe that bottoms out
finite. The hyper opcodes (plain and dynamic) force a lazy list before reading
its items, under the same "not genuinely lazy, or provably finite" gate.

Measured against `raku` afterwards, every case agrees: `gather` and
`gather.map` are `.is-lazy` `False`, `(1..Inf).map` is `True` and still gists
`(...)`, `$g>>.Str` and `$g.map`/`$g.grep` yield their elements, and
`$g.map(*+1).elems` is `2`.

Pinned by `t/lazy-gather-and-junction.t`.
