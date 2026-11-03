# A `whenever` over a filesystem watcher starts watching at the `whenever` statement

`IO::Path.watch` starts at its first tap, as rakudo's does. rakudo taps a
`whenever`'s source the moment the `whenever` runs; mutsu tapped when the
enclosing `supply` block's body returned, and until then the watcher kept moving
its baseline past every change. So a change the block body made after the
`whenever` (Cro's `watch-recursive`: `$d.watch`, `whenever`, then a listing of
the directory) could be lost if it landed between the listing and the tap. The
`whenever` now marks its source's tap as pending, which starts a watcher at once
(what it reports queues for the first tap); the retirement rules still wait for
the real tap. Closes #16198.
