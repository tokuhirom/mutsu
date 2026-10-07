# The metaobject protocol's methods are rows of the HOW classes

The 58 arms of the `.^add_method` / `.^mro` / `.^lookup` dispatcher are now rows of the `Metamodel::ClassHOW`
family of owners in the one method table (ADR-11276 slice 3G). Each handler is a separate `Interpreter`
method, the name list that gated the dispatcher reads the table, and the recognition table and the oracle
snapshot know the seven HOW owners. Behaviour is unchanged; `t/oo/mop/mop-method-rows.t` pins each family
against Rakudo.
