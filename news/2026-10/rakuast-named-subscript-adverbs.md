# RakuAST preserves named subscript adverbs

The RakuAST frontend now converts a subscript carrying an unknown named adverb to the corresponding postcircumfix node and its colonpairs. The parser records the original subscript only when source spelling is requested, so angle indices such as `%h<a>:foo` retain `LiteralHashIndex`. Lowering and the ordinary parser share the CORE candidate-call builder, preserving the error class, source name, and slice descriptor for positional, associative, zen, and multidimensional subscripts.

The frontend ratchet adds the named-adverb suites and a focused RakuAST read/EVAL test. This is an S10 slice of #7564.
