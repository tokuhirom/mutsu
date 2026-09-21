# A metric button and its URL validator landed in different PRs, and only their merge was wrong

Two changes to the bench trend page merged within fifteen seconds of each other on 2026-09-21.
[#8964](https://github.com/tokuhirom/mutsu/pull/8964) added an `allocations` metric (the heap
allocation counts now recorded alongside the instruction series). `#8963` put the page's view state
in the URL, so that a link to "instructions, last 150 commits, as a table" reopens exactly that and a
reload does not silently throw the reader's selection away.

Git merged them without a conflict. The result was still wrong: `#8963` was written against the file
as it stood before the new metric existed, so its validator read

```js
metric: (v) => v === 'seconds' || v === 'ratio' || (v === 'instr' && DATA.hasDet),
```

and `allocs` is not in that list. The failure is quiet in the way that matters. Clicking
**allocations** worked and wrote `#metric=allocs` into the URL; reloading that URL, or opening the
link someone shared, fell back to seconds — the exact loss `#8963` exists to prevent, on the one
metric it did not know about. Confirmed in a browser against the merged page: click writes the hash,
reload comes back showing `0.190s`.

Neither change was at fault and no test of either could have caught it. The button list and the
validator sit about 230 lines apart in the same template, and each PR's own diff was complete and
correct. What was missing was anything that looked at the two together.

So the generator now asserts it. `_assert_metrics_valid()` in `scripts/bench-visualize.py` extracts
the `data-v` of every button in the `metric` group and the `v === '...'` literals of the
`VALID.metric` predicate, and fails the render if either set has a member the other lacks — a button
with no validator entry cannot be linked to, and a validator entry with no button is dead. It runs on
every render, which includes the `pages.yml` deploy, needs no browser and no CI step of its own.
Re-introducing the bug makes it fail:

```
bench-visualize: metric button(s) allocs are not accepted by VALID.metric, so a link to
them cannot be reopened and a reload would fall back to the default. Add them there.
```

The lesson generalizes past this page: when a feature is a *pair* of lists that must agree, and the
two live far enough apart that a merge can satisfy one and not the other, the assertion belongs in
whatever code already walks both.
