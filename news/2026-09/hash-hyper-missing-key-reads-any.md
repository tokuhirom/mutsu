# A Hash hyper reads a missing key as `Any`, quietly

In a union (`»op«`) or one-sided hyper over two Hashes, a key only one side has
is combined with a value for the side that lacks it. mutsu used the operator's
reduction identity there, so `{a => 1} »*« {b => 2}` answered
`{a => 1, b => 2}`. Rakudo reads the missing key through `AT-KEY`, which yields
`Any` (or the hash's `is default` value, or its value type object for a typed
hash), so the answer is `{a => 0, b => 0}`. `+`, `-` and `~` happened to agree
with the identity; `*`, `**`, `/`, `,` and user operators did not.

`hyper_op_pair` (`src/vm/vm_hyper_ops.rs`) now passes each plain Hash's absent
value (`Interpreter::hash_absent_value`) to `hyper_hash_pair`, and the QuantHash
path uses the same value for its Hash side, so `bag(<a b>) »*« {a => 2}` is
`("a"=>2).Bag`. As in rakudo, each per-key application runs with warnings
suppressed, so the `Any` operand prints no "Use of uninitialized value" warning.

Closes [#9564](https://github.com/tokuhirom/mutsu/issues/9564); regression test
`t/lang/operators/hyper-hash-missing-key-any.t`.
