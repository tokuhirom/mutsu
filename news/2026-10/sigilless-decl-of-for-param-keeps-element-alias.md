# `my \q = p` over a sigilless `for` parameter keeps the element alias

A sigilless declaration whose initializer was a `-> \p` parameter of a `for @a` loop bound the element's value, so `$e := q` no longer saw later writes to the element. The declaration now receives the parameter's raw cell, as the plain bind `$e := p` does (#14797).
