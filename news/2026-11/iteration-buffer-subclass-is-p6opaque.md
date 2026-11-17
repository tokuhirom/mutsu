# A plain `is IterationBuffer` subclass is P6opaque

`is IterationBuffer` inherits the methods, not the `VMArray` representation:
in Rakudo a subclass without `is repr('VMArray')` is `P6opaque`, and the
positional `nqp::` ops die on it with `This type (SubBuf) does not support
positional operations` (`elems` for `nqp::elems`). mutsu gave every subclass
element storage at `nqp::create`, so `nqp::push` / `nqp::atpos` accepted it.

`create_instance` now seeds the storage only for `IterationBuffer` itself and a
class that declares `is repr('VMArray')` (the representation is not inherited
either), and the shared REPR check of the positional ops refuses an instance of
a P6opaque subclass (#17627). The check costs nothing for an operand that has
storage. The same mismatch in the `IterationBuffer` methods (`SubBuf.new.push`)
is #17930.
