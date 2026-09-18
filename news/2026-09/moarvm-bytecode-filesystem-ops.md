# NQP filesystem operations now support real distribution code

The NQP compatibility layer now implements the filesystem predicates and
directory-handle operations used by `paths`: `nqp::stat`, `opendir`,
`nextfiledir`, `closedir`, `fileislink`, and `filereadable`. It also supports
`nqp::handle`'s value-preserving CATCH form and `WhateverCode.ACCEPTS`, so
`paths(:file(* eq ...))` evaluates its matcher correctly.

These gaps were found while running `MoarVM::Bytecode` 0.0.27. The focused
regression is `t/vm/nqp-file-stat.t`. The distribution's remaining bytecode
construction performance and backend-identity gaps are tracked in #8732 and
#8733.
