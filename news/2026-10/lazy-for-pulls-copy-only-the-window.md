# A `for` loop over a lazy pipe or gather is linear again

`for (1..*).map(*+0) { last if $_ > N }` took time quadratic in N: 16.7 s at
N = 100000 and 66.9 s at N = 200000 (#10780). A lazy `for` pulls one element
per iteration so that the pipe's callback, or the gather body, interleaves
with the loop body as in Rakudo. But every pull returned a fresh copy of the
whole reified prefix (`cached[..needed].to_vec()`), so iteration *i* cost
O(i). The gather resume path also copied the cache into its take collector
and back out on every pull. A closure sequence (`1, 1, * + * ... *`) cloned
its generation history twice per pull.

The forcing layer now separates *producing* elements from *reading* them.
Each producer (the gather coroutine, the map/grep pipe, the cat-handle
puller, the arithmetic/geometric sequence, the closure sequence, the
triangle reduce) has a fill form that only grows the list's cache. The new
`force_lazy_list_vm_window(list, from, needed)` fills to `needed` and copies
only `from..needed`; `force_lazy_list_vm_n` is its `from = 0` case. The
gather resume path moves the cache into the collector and back instead of
copying it. The closure sequence moves its history out of
`generation_state` for the run. The arithmetic sequence copies only the new
tail.

The lazy `for` loop reads just the chunk it is about to bind. A pipe stage
pulling a lazy source (`pull_source_element`) and a triangle reduce over a
lazy pipe read just the elements they need. Each pull is now O(1) amortized.

On a debug build, `scripts/vm-complexity-check.sh "for over"` now reports a
t(2N)/t(N) ratio of 2.11 for the `.map` pipe, 2.13 for a gather and 1.89 for
a closure sequence, all at N = 20000. Before the change the `.map` pipe's
ratio was 3.94. `t/collections/lazy-seq/for-lazy-pull-window.t` pins the
interleaving, chunking and caching behaviour that the windowed pull must
keep.
