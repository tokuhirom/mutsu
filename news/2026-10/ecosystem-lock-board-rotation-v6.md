# Ecosystem lock board rotated to #12725 (v6)

The ecosystem lock board moved from #11640 to #12725 on 2026-10-11. #11640
had reached 357 comments, well past the ~250 threshold at which a single
`get_comments` read stops being reliable: a 100-comment page of it no longer
fit in one tool response.

The live set was computed with `live-locks.py --check-origin` from all 357
comments.

- **Carried:** nothing. The board starts empty.
- **Not carried:** TOML::Thumb (`claude/happy-turing-l9lshk`) and Heap
  (`eco/heap`). Both had been carried into #11640 from #11256 and were never
  released; both were 185 hours old with no origin branch. Each was released
  on the old board before the rotation.
- Two orphan unlocks needed nothing: a misspelt `CSS::Tagset` that was
  corrected seconds later, and a Template::HAML unlock naming a branch that
  never locked.

#11640 is closed as a redirect to #12725. The `ecosystem:lock` label moved
with the board, and the skills, docs and `sync-working-label.sh` now point at
the new issue.
