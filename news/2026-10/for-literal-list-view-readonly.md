# `for` over a literal list's views binds read-only items

`$_ = 5 for (1,2).values`, `.list`, `.reverse`, `.pairs`, `.sort`, `for (1,2).kv -> \k, \v { v = 5 }`,
`for (1,2) -> \x { x = 5 }` and `.sort` of a bound List used to succeed silently; they now fail like
raku (`X::AdHoc` / `X::Assignment::RO`). The compiler's bare-items oracle accepts a view of a literal
list receiver, `.sort` of a variable is tagged as a loop source so the runtime route decides, and a
sigilless parameter over a bare source is marked as an immutable value. Mutable `Array` receivers still
alias their elements. Closes #10397.
