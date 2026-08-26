# `subbuf-rw` mutates the buffer in place, so the function-call form works

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Type/Buf.rakudoc:84`): `subbuf-rw($b, 2, 1) = Buf.new(42)` exited 0 and left
`$b` untouched, while the method-call form `$b.subbuf-rw(2, 1) = Buf.new(42)`
worked.

## Root cause

`assign_subbuf_rw` (`src/runtime/methods_mut_substr_buf.rs`) was a
*rebuild-and-write-back*: it decoded the buffer's elements, spliced the
replacement over the window, built a **fresh** `Buf`, and stored that fresh
value into `env[target_var]`. That makes the whole operation depend on having
found a variable name to write to — and it severs every other reference to the
buffer even when it succeeds.

The function-call form has no name to work from. `builtins_lvalue.rs` recovers
one by scanning `self.env` for an entry whose value is `values_identical` to
`call_args[0]`; when that scan misses (the buffer lives in a caller local slot
that never reached `env`), `target_var` is `None`, the rebuilt `Buf` is dropped
on the floor, and the assignment is a silent no-op. The method form happened to
survive only because its receiver arrived with a name attached.

The name search was never the real fix, though: `subbuf-rw` is an lvalue on the
**buffer object**, and real `raku` mutates that object — `my $same = $b;
subbuf-rw($b,2,1) = Buf.new(42)` leaves `$same` showing `Buf.new(1,2,42)`, not
the old contents.

## Fix

`assign_subbuf_rw` now writes through the buffer's shared attribute cell via
`value_buf::with_buf_elems_mut`, the same in-place primitive `$b[i] = v` uses,
instead of rebuilding. The mutation is then visible through every alias and is
completely independent of whether a variable name was found — which is exactly
what the nameless function form needed. The env write-back is kept (harmlessly,
now storing the same mutated instance) so a caller holding a stale copy under
that name stays coherent, and the old rebuild path survives only as a fallback
for a `Buf`-shaped instance with no storage node.

This mirrors the `BagHash.add`/`.remove` fix from the same week: adjust counts
in place through the shared `Gc` node rather than rebuild-and-write-back, because
a rebuild severs aliases.

`t/buf-and-list-mutators.t` pins both forms, and asserts identity rather than
value — a second reference to the same buffer must see the write, which a
value-only assertion would have passed on a rebuilt copy. It also covers growing
and shrinking replacements, the no-length form (`subbuf-rw($b, 1)` replaces the
whole tail), and an at-the-end offset.
