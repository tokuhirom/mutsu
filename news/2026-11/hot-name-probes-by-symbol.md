# Method dispatch asks its hot name questions by symbol

A round of AttrX::Mooish's `t/090-concurrent.custom` probe looked names up as
text about 1,300 times (`Symbol::lookup`, a thread-local string-hash probe),
mostly where the caller already held the name as a symbol: method dispatch
passed `class.as_str()` to the per-class method memos, which looked the text up
again to find the symbol they are keyed by. Those callers now probe the memos by
symbol. The static link's `lexically_scoped` test, asked about every name a
linked frame misses (276 times a round), is a memoized symbol flag instead of a
series of string scans, and a qualified variable's package is decided once per
symbol.

The probe's callgrind slope fell from 5,762,273 to 5,674,507 instructions per
round (-1.5%). That is below what this box's wall clock can resolve: six
interleaved runs per side read the same within noise. Part of #18274.
