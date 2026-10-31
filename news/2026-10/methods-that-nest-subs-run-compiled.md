# A method that nests subs runs its compiled body as a callable

The callable `.^find_method` returns, and the value of a class-body `my method`,
ran through the tree-walk carrier whenever the method nested a `sub`: that was
the guard kept after `^add_method` dropped the nested-sub table (fixed in the
previous slice). With the table carried everywhere, the guard is gone, so a
monitor method or any method with an inner `sub` runs the chunk it was compiled
to. A slice of #14572.

The same slice compiles two more registrations once, where they used to leave
`FunctionDef.compiled` empty and run the AST through the carrier: the importable
sub form of an `is export` method (`register_exported_operator_method_sub`) and
the lexical value of a class-body `my method`. The latter is compiled as the
sub form it is (replacing the method-chunk callable of the previous slice), so
it follows the routine path; a `my method` that writes a variable of an
enclosing scope keeps the carrier until the class body can hand it a cell.
