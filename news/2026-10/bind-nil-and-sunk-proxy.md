# Binding Nil keeps it, and a sunk Proxy is read

`my Mu $v := Nil` stored the `Mu` type object (the assignment reset applied to
a bind too), and `my Str $v := Nil` was accepted; a bind now keeps the Nil and
type-checks it like Rakudo (`Mu`/`Any` take it, `Str`/`Int` die). A statement
whose value is a Proxy now FETCHes it, as `.sink` does: `$obj.hash-accessor;`
raises what a lazy attribute's FETCH raises even though nothing uses the value.
Together they let AttrX::Mooish `t/040-errors` "Nils" pass.
