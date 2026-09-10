# Native integer arithmetic wraps at the declared width

In-place `++` and `--` now wrap native integer values before the result is
returned or stored. Native `+`, `-`, and `*` use the matching signed `i64` or
unsigned `u64` machine operation when the compiler can prove both operands are
native integers. Narrow native declarations still narrow their result at the
destination store, while ordinary boxed `Int` arithmetic and out-of-range
ordinary assignments keep their existing behavior.
