RakuAST conversion now preserves an explicit `:exists(False)` or
`:exists(0)` value on array and hash subscripts, and 6.e hash hyperslices
round-trip through `HashIndex` with a `HyperWhatever` index. The array and
hash existence-adverb roast files and hash hyperslice roast file now pass
under the RakuAST frontend (111 assertions together).
