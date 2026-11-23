# DBIish 0.6.9 and NativeHelpers::Blob 0.1.13 are bundled

The database battery is re-vendored from the latest fez releases, verbatim:
`modules/DBIish/` is now `DBIish:ver<0.6.9>` (tag `0.6.9`, commit `1af110d8`)
and `modules/NativeHelpers-Blob/` is `NativeHelpers::Blob:ver<0.1.13>` (tag
`0.1.13`, commit `cec91886`). They move together because DBIish 0.6.9 now
declares `NativeHelpers::Blob:ver<0.1.13+>:auth<zef:raku-community-modules>`.

DBIish's library code is unchanged; the release tightens its dependency list
and fixes `.perl` deprecation noise in its tests. `NativeHelpers::Blob` changed
hands — it moved from `github:salortiz` to the `raku-community-modules` org,
renamed its files from `.pm6` to `.rakumod`, and gained a `license` key in
`META6.json`. Its `MoarVM::Guts::REPRs` was rewritten to survive a moving GC:
`Offset` and `BODY_OF` retry until `.WHERE` is stable and `BODY_OF` returns a
plain snapshot object instead of a live struct view, and the CStruct body
layout is now `{cstruct, child_objs}` — the order mutsu's native-handle body
(ADR-0015) already used. `batteries.lock` follows the new upstream repository,
its commit, and its `t/*.rakutest` suite.
