# The profiler's `hits` column is a transition count, and the documentation said otherwise

The first thing done with `--profile` after ADR-0106 Slice 5 landed was to point it at
a handful of loops and read the exact column. It disagreed with its own documentation.

`docs/profiler.md` said: "A line inside a loop body with no call is entered exactly
once per trip." Measured, on the shipped binary:

```console
$ ./target/debug/mutsu --profile --profile-report=text --profile-kind=line tmp/loopforms.raku
100 100 100 100 100
TOP SELF LINES
   1  tmp/loopforms.raku:15   26.1% self  hits 1   # repeat, 100 trips
   2  tmp/loopforms.raku:9    25.8% self  hits 1   # while,  100 trips
   3  tmp/loopforms.raku:12   24.4% self  hits 1   # loop,   100 trips
   4  tmp/loopforms.raku:2    13.2% self  hits 1   # for,    100 trips
   5  tmp/loopforms.raku:5    10.6% self  hits 1   # for with the body on its own line
```

Every loop ran its hundred trips; every one reports `hits 1`.

The counter is doing exactly what Slice 3 built it to do. `hits` counts line
*transitions* — a line is counted when control arrives at it from a **different**
line — and a loop whose body occupies one line never transitions, so there is nothing
to count. What was wrong was the sentence describing it, which quietly assumed the
body would span a line boundary. Spread the same body over two lines and the
documented behaviour appears: one hit per line per trip, 1,000 each on a 1,000-trip
loop. That is also why `tests/profile_counts.rs` never caught it — ADR-0106 gate 3's
fixture is a `while` with a two-line body, which is the case that works.

Row 5 above is the one worth remembering. Putting the body on its own line is not
enough, because the body is *still* a single line; what matters is whether execution
leaves the line and comes back. The same mechanism makes the loop header inconsistent
across forms for the same reason: gate 3's multi-line `while` reports 5,001 on its
condition line (5,000 trips plus the final false evaluation), while a one-line
`while $i < 100 { ... }` reports 1.

The sampled columns were right throughout — `self_us` and the region split name the
hot line correctly in every fixture above. This was only ever the exact column.

`docs/profiler.md` now describes the mechanism and shows the three shapes side by
side, and says plainly that `hits` is a reliable trip count only where the body is
spread over several lines, so it cannot be compared to NYTProf's statement counts in
general. Whether the counter *should* record a hit on a backedge that lands on the
same line — which would make `hits` a trip count for every loop form, at the cost of
putting work in the counter's cheapest branch and re-opening the ADR-0106 §8 gates —
is [#8737](https://github.com/tokuhirom/mutsu/issues/8737).

Two other records were corrected in the same pass. ADR-0106's status line still read
"Slices 0-3 shipped, Slices 4-5 not started" while section 9 of the same file
described both as shipped, and the ADR index still carried "implementation not
started"; both now read "Accepted (Slices 0-5 shipped; Slice 6 optional and
unstarted)". And the ADR's three open questions and its optional Slice 6, which
existed only as prose inside the ADR, now have issues to be found from outside it:
[#8738](https://github.com/tokuhirom/mutsu/issues/8738) (Slice 6's output formats),
[#8739](https://github.com/tokuhirom/mutsu/issues/8739) (`sampled_ns` summing past
`wall_ns` on a threaded run), [#8740](https://github.com/tokuhirom/mutsu/issues/8740)
(per-line allocation attribution) and
[#8741](https://github.com/tokuhirom/mutsu/issues/8741) (whether a `start` block folds
into the spawning line's inclusive time).
