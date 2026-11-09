# `.CREATE` on a defined invocant allocates a new object of the same class

`Mu.CREATE` is `nqp::create(self)` in rakudo, so `self.CREATE!SET-SELF(...)` inside a
`clone` override works on an instance. mutsu only handled a type object and died with
`No such method 'CREATE' for invocant of type ...` (BinaryHeap's `clone`; its
`t/01-binaryheap.rakutest` is now 10/10, like raku).
