# `.Seq` of a type object reads the class's own `iterator`

`MinHeap.Seq` (and so `MinHeap.list` of an uninitialized `my MinHeap $heap`) answered
`(MinHeap,).Seq` because the structural `.Seq` coercion treated every type object as a bare scalar.
`Any.Seq` is `Seq.new(self.iterator)`, so a type object of a class that defines `iterator` now reads
it, like an instance already did; a core type object (`Int.Seq`) is unchanged. Found running
BinaryHeap's own suite next to rakudo.
