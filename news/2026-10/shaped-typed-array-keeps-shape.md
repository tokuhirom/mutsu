# A typed multi-dimensional array keeps its shape through element coercion

`my int @a[2;2]` and `my Int @a[2;2]` coerce the declared array's elements to
the element type. That coercion rebuilt the whole array's backing data without
the array's `shape`. The rows carry no shape of their own, so the shape could
not be inferred either. A native one then had no `.list` at all ("No such
method 'list' for invocant of type 'Array'"), and `.List` returned its rows
instead of its leaves. The rebuilt array now keeps its shape. Its rows still
leave theirs off, which keeps element writes on the fast path.

PDF's `t/io-util.t` compares `$out.&unpack(@W).list` with `@in.list` on a
`my uint64 @in[4;3]`, and died there (#14243, #12907).
