# Method calls push only the routine name as their samewith context unless the program can read more

Every compiled method call pushed a samewith context holding the method name (a `String`), a clone of
the invocant and a copy of the call's arguments (`args.to_vec()`), plus a metamodel-dispatch check.
Only `samewith`, the deferral builtins (`callsame`, `nextsame`, ...) and the gather-body capture of
`samewith` read the invocant and arguments; the one other reader is a parameter-binding error message
that wants the name.

A new process-global latch, `opcode::samewith_context_possible()`, is set when any compilation unit's
source text (or constant pool string) mentions `samewith`, and is implied by the existing
dispatcher-builtin latch. While it is unset, `push_method_samewith_context` pushes the name only:
no argument copy, no invocant clone, no metamodel probe. A program that names `samewith` anywhere
(zef and the bundled Digest modules do) keeps the full context, exactly as before.

Measured with callgrind on a 40,000-iteration `$o.plain(1)` loop, profiling build, the same binary on
the same script with and without a `# samewith` comment: 630.96M -> 618.27M Ir (-2.0%, ~317 Ir per
call). The `trie-create` goal in #15235 is not met by this slice.
