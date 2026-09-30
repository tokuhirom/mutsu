# A typed pointy-block parameter no longer poisons the caller's same-named variable

A closure's exit writeback skipped its parameter names but not their
`__mutsu_type::<name>` constraint metadata, so `-> blob8 $h { $h.elems }` called
inside `f` left `expected blob8` on a `my UInt $h` in `f`'s caller (#9965). The
parameter's type and hash-key-type metadata keys are now part of the
call-local name set (`ParamNameSyms::call_local`).
