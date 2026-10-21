# The container `Attribute.auto_viv_container` hands out enforces the `where` clause

A Scalar from `Attribute.auto_viv_container` carried the attribute's type but
not its `where` clause, so a store into it (`$!val = $value`, as AttrX::Mooish
does with a lazy builder's value) was never refused. The cell's constraint now
holds the clause with the scope it was written in, a store through the cell
checks it and reports `X::TypeCheck::Assignment`, and `nqp::clone_nd` keeps it.
AttrX::Mooish `t/040-errors` "Base Errors" passes.
