# #9156 closed; the native-array shift remainder moves to #9695

#9683 met every ratio goal in #9156: queue push+shift, multi-element prepend,
by-value `%h<k>.unshift`, front `splice`, the `splice(0, 1)` loop, the native
int shift loop and sigilless `$a.unshift`. The two `Rakudo: O(..)`
suffixes left on `ArrayData::shift_front` and `ArrayData::insert` cover only
`NativeBacking` (ADR-0030) arrays. Only a native array after `.WHERE` and a
CStruct `HAS` array get promoted to that storage. On them a shift loop is
still quadratic: 100,000 elements take 43.6 s where raku takes 0.055 s.

That remainder needs a design decision, because C can see the native buffer's
address. It also cannot be reached from anything Rakudo lets you shift, and no
known workload front-consumes a native array after taking its address. It is
now its own `todo:perf` issue, #9695, in the icebox. The two suffixes point
there, and #9156 is closed.
