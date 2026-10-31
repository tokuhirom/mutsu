# A class-body `my method` value runs its compiled chunk

The code object of a `my method` declared in a class body (the `:&FETCH, :&STORE`
of a `Proxy` subclass) used to run through the tree-walk carrier of
`call_sub_value`, recompiling its AST body at every call. It now runs the
method's own compiled chunk when the method nests no subs and writes no variable
of an enclosing scope; the other methods keep the carrier. This is a slice of
#14572.
