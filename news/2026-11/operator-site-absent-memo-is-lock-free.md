# The operator-site "no import binds this" memo is read without a lock

`OpCode::LexImportBuiltinOp` remembers per site, for one import-set generation,
that no `use` block binds a user candidate for the built-in operator. That
memo (#16565) was a `Mutex` taken on every execution of the site. It is now a
sequence-locked `(generation, chain)` word read with plain atomic loads; a
writer that loses a race simply drops its update, since the word is only a memo.

Measured with `use Cro::HTTP2::RequestParser` loaded and a `$s += %h<a>` loop
(profiling build, loop-only callgrind, slope 3,000 -> 9,000 iterations): 4,572
-> 4,537 Ir/iteration (-0.8%). The rest of the site's cost (~75 Ir in
`exec_lex_import_builtin_op_site`, ~80 in the JIT shim) needs the JIT to test the
generation inline; that is the next slice on #7817.
