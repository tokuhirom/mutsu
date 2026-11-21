# An element bind answers the bound value, not an itemized copy

`(%h<a> := (1, 2).Seq)` is the `Seq` itself in Rakudo, as the element now is. mutsu stores a
`:=`-bound element in a shared cell, and handed that cell out as the expression's value, which
reads back as a `Scalar` holding the value: `$((1, 2).Seq)`, `$[1, 2]`. A routine that returned
`%cache{$k} := $seq` (Backtrace::Files' `path-lines`) thus gave its caller one item, which an `@`
assignment kept as a single element (#18260). The single-key, chained (`%h<a><b> := v`) and
multi-dimensional (`%h{1;2} := v`) element binds now answer the bound value; a bind of a variable
(`%h<k> := $x`) still answers that variable's container, so assigning to it writes through.
