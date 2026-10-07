# Caller topic lexical writeback

`CALLER::LEXICAL::<$_> = value` now writes the immediate caller's topic through its saved environment and local slot. The write is consumed at that call boundary, so a method's per-frame topic cannot leak through intervening frames. A regression test covers the `DirHandle.read` loop pattern.
