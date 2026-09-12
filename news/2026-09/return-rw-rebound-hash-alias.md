`return-rw` now preserves a live hash-element container when a routine
rebinds a name onto one of its own nested elements. Plain assignment through
the rebound name is writable as well.
