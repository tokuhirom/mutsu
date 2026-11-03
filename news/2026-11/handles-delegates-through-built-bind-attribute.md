# `handles` delegates through an `is built(:bind)` attribute

`has $.keys is built(:bind) handles <elems Numeric>` keeps the caller's container in
the attribute, and the delegation forwarded the container itself instead of the value
in it, so `+$obj` died with `No such method 'Numeric' for invocant of type 'Array'`.
The delegate is now dereferenced first. Array::Unsorted::Map `t/01-basic.rakutest` goes
from 2 to raku's 34 ok.
