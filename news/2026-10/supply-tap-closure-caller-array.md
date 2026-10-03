# A supply tap closure keeps its sub's own array when the caller has a same-named one

The program below printed `1` and then `2` instead of `1` and `1` (#11345):

```raku
use Test;
sub rc() { my $s = supply { ... }; my @log; $s.tap: -> $v { @log.push($v) }; ...; @log }
for 1, 2 { my @log = rc(); say @log.elems }
```

On the second call, the tap pushed onto the caller loop's `@log`.

Mainline lexicals are visible by name once something like `use Test` or an
`EVAL` needs them. When the `supply` block closure was created, before the
sub's own `my @log` was declared, it captured the `@log` visible at that
point, which was the caller's. Running the supply body merges that captured
env into the calling frame. That merge already let the caller's array win
over a captured array the closure does not own. It did not do so when the
caller's array had been boxed into a shared cell, which is what the tap
closure's capture of `@log` does. So the stale array replaced the sub's own
binding, and the next thread spawn published it. The rule now treats a
caller cell like a caller array.
