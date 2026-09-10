# The HTTP/2 request-parser battery test races with itself, so the gate stops running it

`cro-http`'s `t/http2-request-parser.rakutest` is out of the release gate until
[croservices/cro-http#217](https://github.com/croservices/cro-http/pull/217)
lands. Not because mutsu fails it — because the file's own harness is a race, and
a racing file on a release gate turns a green build into a coin flip.

## The defect is in the test, not under it

`test()` runs each request's checks inside a `start` block and calls `ok` from
there:

```raku
start {
    for @checks[$current-counter].kv -> $i, $check {
        ok $check($request), "check {$i + 1}";
    }
    $test-completed.keep if $current-counter + 1 == $count;
    ...
}
```

`Test` is not thread-safe. With two concurrent HTTP/2 streams the requests
interleave their TAP output, and a test can leak past the `pass` that should
follow it — so the plan and the emitted count disagree and the file fails.

The upstream fix has each request record its results into a `Promise` and reports
them from the main thread in request order:

```diff
-    await Promise.anyof($test-completed, Promise.in(5));
+    await Promise.anyof($test-completed, $all-checks-recorded, Promise.in(5));
```

## Why it only shows up now

Mutsu's native `Test` provider is Rust and serializes, so the race has never been
visible in the gate. The vendored `Test.rakumod` is Raku code with a plain
counter, and it is not:

| provider | passes |
| --- | --- |
| native (what the gate runs today) | 20/20 idle, 20/20 with three CPU burners on a 4-core box — 61/61 assertions every run |
| vendored `Test.rakumod` (`MUTSU_REAL_TEST=1`) | 29/30 idle, **11/20** under the same load |

With the `MUTSU_REAL_TEST=1` flip due shortly, the gate is about to start running
the second row. Ten percent of that column is a release blocked by somebody
else's test bug, so the row comes out ahead of the flip rather than after the
first red release.

## The exclusion list grew a second category

`batteries-exclude.txt` had one bar: the file must reach outside this machine
unconditionally (an httpbin.org outage must not block a release). This is a
different shape of the same principle — *the file's verdict is not a statement
about mutsu* — so the list now states two categories, and the new one carries its
own three-part bar: root-caused to the upstream file, reported upstream with the
PR named, and a written restore condition. An entry that cannot name a fix in
flight does not qualify; that is a mutsu bug to fix.

It is deliberately not a `flaky-tests.txt` entry. That ledger re-runs a
quarantined test up to three times, which is the right answer for a bounded
statistical flake — a Binomial assertion a correct RNG violates now and then.
Here the battery harness has no retry path at all, and a ~50% per-run rate would
not converge in three attempts if it had one.

Because `--update` skips excluded files too, the row cannot drift back into the
baseline on its own. Deleting the entry is the only way back in, which is what
makes the restore condition load-bearing: when #217 is merged and
`batteries.lock`'s Cro::HTTP pin moves past it, delete the entry, re-run
`--update`, and check the row is back in `batteries-whitelist.txt`.

## What the chase left behind

Characterizing this race precisely enough to see that it *was* one produced five
general interpreter fixes, none of them HTTP/2-specific: #7786 (a pure-read
method probe took the registry write guard), #7796 (~50 symbol tables deep-copied
per thread clone), #7800 (candidate matching bound into the frame env), #7801 (a
`whenever` body recompiled per emitted value) and #7812 (the carrier compile cache
keyed by an identity that is fresh per call). A HEADERS frame went 23.4 ms to
10.3 ms and a DATA frame 2.12 ms to 1.23 ms along the way.

Tracked as item 3 of [#7555](https://github.com/tokuhirom/mutsu/issues/7555).
