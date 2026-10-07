# Seeking an IO::Handle clears buffered words

`IO::Handle.seek` now clears words read ahead by a prior limited
`IO::Handle.words` call. Reading words after a seek starts at the new file
position instead of returning stale words from the old position.

Fixes #12186.
