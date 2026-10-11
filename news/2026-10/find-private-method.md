# `.^find_private_method` and `.^private_method_names`

`Metamodel::ClassHOW` now answers `find_private_method` (the class's own private `Method`, or
`Mu`) and `private_method_names`, as method-table rows beside `private_method_table` (#12788).
LLM::Chat's `t/15-streaming-resilience.rakutest` calls `.^find_private_method` on load.
