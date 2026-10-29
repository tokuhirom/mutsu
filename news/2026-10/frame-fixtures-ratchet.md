# A ratchet keeps `self`, `?CLASS` and `?ROLE` out of the runtime's name lookups

`make check-frame-fixtures` (in `make checks` and CI) counts, per file under the runtime layers,
every spelling of the three frame-record names -- the string literals `"self"` / `"?CLASS"` /
`"?ROLE"` and `wk::self_()` / `wk::class_decl()` / `wk::role_decl()` -- and fails when a count rises
above `scripts/frame-fixtures-baseline.txt` (#14843, ADR-14843). A reference to one of them is a place
that could grow an `Env` read or write of a name the frame record owns; a new one has to be
justified in review. Counts only go down: re-cut the baseline with
`scripts/check-frame-fixtures.py --update` after removing a reference.
