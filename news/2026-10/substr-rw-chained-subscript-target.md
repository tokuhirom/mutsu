# `.substr-rw(...) = v` through a chained subscript writes back

`%h<a><b>.substr-rw(3, 1) = 'x'` mutated a detached copy of the element, so a
chain of two or more subscripts lost the write (only a single subscript was
written back). The element is now stored back through the same chain. PDF
0.6.17 `cos-type-objstm` passes.
