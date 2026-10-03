# Ecosystem lock board rotated to #11256 (v4)

The ecosystem lock board moved from #10045 to #11256 on 2026-10-03. #10045
had reached 269 comments, past the ~250 threshold at which a single
`get_comments` read stops being reliable.

The live set was computed with `live-locks.py --check-origin` from all 269
comments.

- **Carried:** TOML::Thumb, highlighter, Badger and Rake, each with its
  holder's branch and original lock time.
- **Not carried:** Pod::To::HTML, Object::Permission and Net::Postgres. All
  three were stale: locked more than 100 hours earlier, with no origin branch.
  Each was released on the old board before the rotation.

#10045 is closed as a redirect to #11256. The `ecosystem:lock` label moved
with the board, and the skills and docs now point at the new issue.
