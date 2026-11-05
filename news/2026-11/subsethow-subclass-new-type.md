# A `Metamodel::SubsetHOW` subclass's `new_type` mints a real subset

A user subclass of `Metamodel::SubsetHOW` whose `new_type` delegates with
`callwith`/`nextwith` now reaches the native subset-minting `new_type` (the
subset metaclass joins the classes that get a metamodel dispatch frame), so the
`:refinement` is kept and `.^refinee` answers. The qualified form
`self.Metamodel::SubsetHOW::new_type(...)` no longer re-enters the override
until the stack is exhausted; it calls the native metamethod (#16347).
