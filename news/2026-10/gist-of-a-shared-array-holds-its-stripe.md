# `.gist` of an array another thread is shrinking no longer panics

`Array.gist` trims a long array to its first 101 elements before rendering
(`gist_head`). It tested the array's length and then sliced from the node's
live start (`head`), with nothing held, while another thread could `shift` or
`splice` the same node: the slice start passed the new end and the process died
with `range start index 51 out of range for slice of length 0`, in about one
release run in six of the `@x` reader harness. The length test and the copy of
the head now run under the node's ADR-0068 stripe. The harness itself is back
in the suite as `t/concurrency/concurrent-param-array-reader-families.t`: the
twin of the hash one, pinning only that every read family finishes (#13632).
