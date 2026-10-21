# A `$` parameter's default is an item, like a supplied argument

`sub f($a = [])` used to bind the bare Array `[]` when the argument was
omitted, while a supplied Array binds as an item (`$[...]`). The difference
showed wherever itemization matters. With the bare Array, `%h{$a}` was a slice
of zero keys answering `()` instead of a single-key lookup answering `Any`, and
`$a.raku` printed `[]` instead of `$[]`. The positional and named default
paths now itemize the default the way a supplied argument is itemized. This
happens after any coercion, so `List() :$array = []` binds `$[]`.

PDF's `PDF::COS::Array.new(List() :$array = [], |c)` checks `%seen{$array}`
for an array it is already building. The slice answered `()`, which `with`
treated as a found object, so `new` returned that empty List instead of a new
array (#12907).
