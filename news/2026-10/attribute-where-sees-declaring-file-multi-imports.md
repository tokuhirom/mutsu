# An attribute `where` closure sees the declaring file's imported proto/multi routines

`class X::C { use X::Y; has Str:D $.ip is required where { chk($^a) } }` (Net::BGP's
`Peer`) died with "where constraint failed" for a valid value when `chk` was an imported
`proto`/`multi` family: the family's visibility is per compunit, but the predicate's
closure was built under the caller's frame, so it belonged to the caller's unit and `chk`
resolved to "Unknown function" (swallowed as a failed constraint). The check now runs under
a block frame stamped with the declaring file, like attribute defaults already do.
