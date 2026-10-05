# A lexical class method keeps its own type name

A method of a lexical type now resolves its own name through the declaration that created it.
This keeps `RIS` bound to a `my class` when a method runs after its declaring block has exited,
including when an outer constant named `RIS` is visible to the caller. Methods composed from a
`my role` keep that role's name as well.

The method compiler records the lexical type name as a free read, and declaration-time method
capture stores that type object under the source-facing name. The regression test covers a
shadowed outer constant, an unshadowed lexical class, and a composed role method.
