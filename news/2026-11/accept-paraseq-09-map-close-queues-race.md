# ParaSeq t/09-map joins the accepted close-queues race

ParaSeq 0.2.7's `t/09-map.rakutest` timed out in the 2026-11-24 evening sweep. Its record has
flipped between pass and timeout across recent ledgers. `.map` schedules through ParaSeq's
`sub fast()`, the same path as `t/08-grep.rakutest` and `t/20-unique.rakutest`. That path can
deadlock when a batch finishes before `$result.close-queues` runs.

On raku 2026.11, a `sleep 0.05` injected before that call hangs the file in 5 of 5 runs, right
after test 20; unmodified, it passes 3 of 3. A mutsu release build passes 5 of 5 runs alone and
hangs 1 of 18 with three copies running at once.

The file is now an accepted divergence under #17723, as the other two are (maintainer decision
2026-11-24).
