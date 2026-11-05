# An array-backed iterator no longer copies its items on every pull

`Interpreter::array_iterator_call` (the protocol methods of the built-in
`Iterator` over a list: `pull-one`, `skip-one`, `push-all`, ...) copied the whole
`items` array into a fresh `Vec` on every call, so a `pull-one` loop over n
elements cost O(n^2) although the method says O(1). It now reads the array in
place through a handle clone and copies only when a lazy source is topped up.

ParaSeq 0.2.7 `@list.&hyperize(16, 3).Seq.elems` (#16475, `Seq.new:
self.iterator` over a buffered iterator): 20000 elements 22 s to 0.27 s on a
profiling build; callgrind 2000 to 4000 elements went from +2.19G to +0.14G
instructions (4x per doubling to linear).
