# A damaged precompilation entry is a cache miss

A compiled-module cache entry now carries a checksum of its AST and compiled
sections, checked when the entry is loaded. A file damaged after it was written
(a truncated write, a flipped bit) is removed and the module is recompiled,
instead of the process exiting with status 70 when a routine body fails to
decode on its first use, long after the module loaded. (#14553)
