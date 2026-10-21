# Binding Nil keeps it, and a sunk Proxy is read

`my Mu $v := Nil` stored the `Mu` type object (the assignment reset applied to
a bind too), and `my Str $v := Nil` was accepted; a bind now keeps the Nil and
type-checks it like Rakudo (`Mu`/`Any` take it, `Str`/`Int` die). A non-rw
generated accessor now FETCHes a Proxy its attribute is bound to, as Rakudo's
decontainerized accessor return does: `$obj.hash-accessor;` raises what a lazy
attribute's FETCH raises even in statement position (a fast accessor read
declines a Proxy in the slot so the full dispatch reads it).
Together they let AttrX::Mooish `t/040-errors` "Nils" pass.
