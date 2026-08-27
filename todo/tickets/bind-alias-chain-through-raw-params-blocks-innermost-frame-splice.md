# A `:=` bind does not propagate transitively through raw-parameter aliases, so the ancestor-frame splice must stay a blanket by-name write

Found while fixing
`todo/deep/bind-propagate-ancestor-frames-clobbers-unrelated-recursive-locals.md`
(now `news/2026-08/bind-propagate-ancestor-frames-frame-ownership-gate.md`).

## What is going on

`Interpreter::propagate_bind_to_ancestor_frames` (`src/vm/vm_var_assign_ops.rs`)
writes a `:=` bind's shared `ContainerRef` into **every** ancestor call frame
whose `saved_env` declares the source name in its own tier. Only the innermost
such frame is the scope the bind's source actually resolved in — the ones
further out hold variables the source is *shadowed by*, not aliases of it — so
by rights the loop should splice once and stop.

It cannot, today. `roast/S32-list/tail.t` and `roast/S32-list/skip.t`
(the `PredictiveIterator` subtests, "tail makes use .count-only when it is
implemented") depend on the blanket write:

```raku
my $pulled = 0;
sub make-seq ($i = 0) {
    Seq.new: class :: does PredictiveIterator {
        has $!pulled;
        method !SET-SELF (\pulled, $!i) { $!pulled := pulled; self }
        method new       (\pulled, \i)  { self.bless!SET-SELF: pulled, i }
        method pull-one  { $!pulled++; ... }
        ...
    }.new: $pulled, $i
}
```

`$!pulled := pulled` runs in `!SET-SELF`, whose `pulled` is a **raw parameter**
— an alias of `new`'s `pulled`, which is itself a raw parameter aliasing
`make-seq`'s argument, which is the block's `my $pulled`. mutsu does not
propagate a bind transitively along that alias chain: each raw parameter's own
aliasing is not followed. The only thing that gets the cell all the way out to
`my $pulled` is the blanket by-name splice hitting *every* frame that happens
to declare `pulled` — and it works here only because the roast author named the
outer variable the same as the parameter. Change the outer variable's name and
the write no longer reaches it, on `main` as much as after the fix:

```sh
timeout 10 ./target/debug/mutsu -e '
my $counter = 0;
class C { has $!p; method set(\pulled) { $!p := pulled }; method bump { $!p++ } }
my $o = C.new; $o.set($counter); $o.bump; say $counter'
# mutsu: 0    raku: 1
```

So the blanket splice is standing in for a mechanism that does not exist, and
the roast tests pass by a name coincidence.

## Consequences

Two things are blocked on fixing this properly (making a raw/`is rw` parameter
bind the caller's container, so a `:=` through it joins that cell and needs no
by-name reach at all):

1. **`propagate_bind_to_ancestor_frames` cannot stop at the innermost matching
   frame.** Stopping there is otherwise correct and was measured to fix one
   further shape — a `:=` bind performed from a **closure** nested inside a
   recursive routine:

   ```raku
   my @levels;
   sub rec(Int $n) {
       my $v = $n;
       if $n > 0 { rec($n - 1) } else { my $c = { my $x := $v; $x = 999 }; $c() }
       @levels.push($v);
   }
   rec(3);
   say @levels;   # raku: [999 1 2 3]   mutsu: [999 999 999 999]
   ```

   The closure's own compiled code has no slot for `$v`, so the frame-ownership
   gate cannot see that the source is the enclosing invocation's own lexical,
   and the by-name splice reaches every recursion level. Verified: the
   innermost-frame-only rule fixes exactly this and nothing else regresses
   except the raw-parameter chain above.

2. **The frame-ownership gate has to make an exception for parameter slots.**
   `bind_source_is_own_frame_lexical` deliberately treats a source that lives
   in a `code.param_local_slots` slot as *not* this invocation's own lexical,
   purely so the chain above keeps working. Once raw parameters bind the
   caller's container, that exception should go, and a bind whose source is a
   **by-value** parameter would stop clobbering every recursion level's copy —
   measured still divergent today:

   ```raku
   my @levels;
   sub rec($n is copy) {
       if $n > 0 { rec($n - 1) } else { my $x := $n; $x = 999 }
       @levels.push($n);
   }
   rec(3);
   say @levels;   # raku: [999 1 2 3]   mutsu: [999 999 999 999]
   ```

## Suggested shape

Make raw (`\p`) and `is rw` parameter binding install the caller's container
(the existing `ContainerRef` cell, or promote the caller's variable to one) in
the callee's parameter slot, so that `$!x := p` reuses that cell through the
ordinary source-cell reuse already implemented in
`vm_var_assign_set_local.rs` / `vm_exec_dispatch.rs`. Then drop the parameter
exception and the blanket loop together, and add the closure-in-recursion case
to `t/bind-alias-recursive-frame-index.t`.
