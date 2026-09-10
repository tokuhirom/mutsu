# The supply FIFO pin, measured on the four-core box that reported it

[#7838](https://github.com/tokuhirom/mutsu/issues/7838) reported
`t/concurrency/supply/supply-serialize-fifo.t` test 3 failing about half the
time in a 4-core remote container, and asked the right question about it: a pin
that only holds on machines with enough cores is not a pin. The issue was filed
against `main` at `a3fb606`, an hour before the fix for
[#7831](https://github.com/tokuhirom/mutsu/issues/7831) landed, and describes
the same ordering hole — the already-resolved promise path taking no
`SupplyTicket`, so it ran the reaction inline ahead of every reaction already
ticketed and queued. `news/2026-09/already-resolved-promise-supply-ticket.md`
has that story.

What was still missing is the part #7838 actually asks for: the fix was measured
on a 12-core dev box, not on the hardware where the symptom was loud. Re-measured
here on a 4-core container, release build, at `66a1e4e`:

| binary | runs | test 3 failures |
| --- | --- | --- |
| `main` with the fix | 30 serial + 60 under 4-way parallel load | **0 / 90** |
| the same tree, ticket reservation reverted | 20 serial | **18 / 20** |

A stray run added one more data point for free: a full `prove -r t/` sweep was
launched against the reverted binary by mistake, and test 3 was the single
failure in 3952 files and 42022 tests. So the pin fires under the real suite's
load too, not only when the file is run on its own. The same sweep on the
rebuilt binary is green.

So the hole is closed on the reporting hardware, and the pin is not merely
green there — it is ~90% sensitive to the regression there, which is a good deal
better than the ~1-in-6 rate the fix was originally chased at. There is nothing
to quarantine: `flaky-tests.txt` correctly does not list this file, and
`docs/flaky-test-policy.md`'s standard for not relabelling a deterministic bug
as noise was the right call.

#7838 offered a second reading to rule out — that the ordering is not in fact
guaranteed, and the assertion, not the interpreter, is what is wrong. It is not:
`raku` v2026.07 runs the same file green 5 times out of 5 on this same 4-core
container, all four subtests. Rakudo really does publish these reactions in the
order their sources produced them, so `1 2 3 4 5 6` is the right expectation and
mutsu now matches it.

The reverted run also settled which subtest carries that sensitivity, which was
not obvious and is now written into the file. All 18 failures were test 3 alone;
tests 1, 2 and 4 stayed green through every one. Test 4 — added by the #7831 fix
to exercise the already-resolved path on its own — *cannot* catch a return to the
inline path, because when every reaction runs inline on the registering thread it
runs in registration order and comes out trivially sorted. It pins the ticket
reservation that replaced the inline path, which is worth having, but test 3's
alternation of already-kept and later-kept promises is the shape that detects the
bug. Folding the two together would quietly retire the only sensitive pin in the
file.

Closes #7838.
