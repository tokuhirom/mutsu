# An omitted `:D` optional parameter dies instead of binding its type object

`sub f(Str:D :$d) { }; f()` and `sub g(Str:D $x?) { }; g()` now die with
`X::Parameter::InvalidConcreteness` ("Parameter '$d' of routine 'f' must be an
object instance of type 'Str', not a type object of type 'Str'. Did you
forget a '.new'?"), in subs and methods alike, as Rakudo's binder does. The
general binder and the method fast path bound the omitted parameter's type
object without looking at the `:D` smiley. The check sits in the one place
every binder already asks about an omitted optional's implicit value
(`check_omitted_optional_value`, formerly only a subset check); a `:D`
parameter with a default and a `:U` or unsmileyed one are unchanged (#18075).
