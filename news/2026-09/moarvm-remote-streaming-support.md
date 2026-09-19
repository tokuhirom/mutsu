MoarVM::Remote's ecosystem tests exposed several interpreter gaps in live
streams. `Supply.share` now broadcasts cold on-demand sources correctly,
discarded supply callback results are evaluated, nested byte sequences are
flattened by `Buf`/`Blob` constructors, and Pair patterns smartmatch their
values. Closure continuation writeback also keeps nested streaming parsers
alive across input chunks. The remaining debugger/reactor gaps are tracked in
#8825 and #8826.
