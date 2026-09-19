# A `gather @list.map: *.take` now yields every element to a lazy consumer

A `Seq` produced by `gather @list.map: *.take` (any spelling — colon-call,
paren-call, a `gather { ... }` block) used to yield only **one** element to a
later lazy consumer (`.map`, `.elems`, ...), instead of one element per item
in `@list`. Eagerly reifying the same `Seq` into an `Array`
(`my @a = gather @list.map: *.take;`) always worked, and `gather for @list {
take $_ }` also worked lazily — only `gather` wrapping a nested `.map()`
whose block called `take` broke, and only when the result was consumed
lazily one pull at a time.

The root cause: the eager `.map`/`.grep`/`.first` fast paths
(`eval_map_over_items` and friends) run their whole loop over the source
items inside `with_nested_registers`, a boundary that clears the call-frame
stack for isolation and has no way to snapshot/resume mid-iteration (unlike
a bytecoded `ForLoop`, which records a `ForLoopResumeState` for exactly this
purpose). When a `take` inside such a loop hit the gather's take-limit
while being pulled one element at a time, `take_value` treated it as an
unnested take and raised the suspend signal immediately — but with no
resume state to attach, the gather body's driver treated the whole
statement as finished after only the first `take`, permanently marking the
coroutine done with just one element collected.

The fix makes `with_nested_registers` defer take-limit suspension
(`lazy_take_boundary_defer`) for its whole duration, the same way a
condition-driven `while`/C-style loop already defers to its own iteration
boundary. Since this boundary genuinely cannot resume mid-loop, the
nested run now simply runs to completion in one pass instead of aborting
early — the same shape `deepmap`'s (already correct) take-inside-a-real-call
already had.

Regression test: `t/collections/lazy-seq/gather-map-take-lazy-resume.t`.

Closes [#8783](https://github.com/tokuhirom/mutsu/issues/8783).
