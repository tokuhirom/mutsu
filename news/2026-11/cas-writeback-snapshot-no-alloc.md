# `cas $x, &code` stops allocating a name per env entry

A `cas` whose block may write a captured outer lexical (`cas $head, -> $n {
$was = $n; ... }`) snapshots the frame env before the block runs and diffs it
afterwards. The diff tells the call site which caller variables to write
back. Both passes resolved every env key into a fresh `String`, once to filter
the snapshot and again to filter the diff, and the snapshot went into a
SipHash map.

The two passes now read each key's interned text (`Symbol::as_str`), and the
snapshot goes into an Fx map. The set of names written back is unchanged.

On Concurrent::Stack, one push plus one pop (two `cas` calls with blocks)
went from 224.9k to 221.3k instructions (callgrind slope, profiling build).
This is a bounded cleanup, not progress on #11482's ratio. The two block calls
themselves are ~100k of that pair.
