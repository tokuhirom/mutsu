# A method call on a bare-word call of an imported raw routine FETCHes its Proxy

`errno.^name` and `errno.symbol`, where `errno` is an `is raw` routine imported through `sub EXPORT` that answers a `Proxy`, now read the FETCHed value like the parenthesised call does (the bare-word receiver went through the mutating-method opcode, which never FETCHed). Universal::Errno `t/errno.t6` passes.
