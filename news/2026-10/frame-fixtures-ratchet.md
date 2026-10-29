# A ratchet keeps `self`, `?CLASS` and `?ROLE` out of the runtime's name lookups

`make check-frame-fixtures` (in `make checks` and CI) counts, per file under the runtime layers,
every spelling of the three frame-record names -- the string literals `"self"` / `"?CLASS"` /
`"?ROLE"` and `wk::self_()` / `wk::class_decl()` / `wk::role_decl()` -- and fails when a count rises
above the number in the file for that source file under `scripts/frame-fixtures.d/` (#14843, ADR-14843). A reference to one
of them is a place that could grow an `Env` read or write of a name the frame record owns; the count
is an inventory, not proof that no `Env` key exists. A new one has to be justified in review. Counts
only go down, and a drop needs no baseline edit, so parallel PRs never conflict on the ratchet;
`scripts/check-frame-fixtures.py --update` prunes the files after references are removed.
