# Pin the multi-dimensional hash read result

An associative multi-dimensional subscript with scalar keys keeps its
one-element `List` when passed as a routine argument, matching its direct read
and Rakudo. The bind-capable VM opcode now carries the subscript kind and wraps
the selected leaf in a slice container. A raw parameter can still write
through that leaf.
