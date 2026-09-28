# Pod::To::HTML: every baseline test file passes

`Pod::To::HTML` v0.8.1 had 11 of its 15 rakudo-baseline test files at parity.
Now all 15 pass. Five interpreter bugs were behind the other four files, and
none of the fixes is specific to the module.

**Accessor subscript stores with a list index.** `$obj.h{$k} = v`, `$obj.a[0, 1] = …`
and `$obj.h{@keys} = …` go through `__mutsu_index_assign_method_lvalue`. That
builtin read every list-shaped index as `[i;j]` coordinates, but the compiler
already lowers a real multi-dim subscript to its own opcode. So an itemized array key
died with "Multi-dimensional index on non-array container", and a positional
slice nested the RHS (`$c.a[0,1] = 5,6` gave `[[(Any) (5 6)]]`). Now a
non-itemized list, Seq or finite Range subscript is a slice: one element store
per key, zipped with the flattened RHS and padded with `Nil`. An itemized one
is a single key, as in the named-variable store. The dead
`multidim_assign_nested` helper is gone.

**Pod implicit code blocks follow rakudo's virtual margin.** An indented
paragraph is a code block only when it is indented further than the last
text-bearing directive (`=head`, `=item`, `=defn`, a named block, or the `=for`
form of one). `=begin`/`=end`, `=comment`, `=config`, `=code` and `=table`
leave the margin alone. Before, any indentation made a code block, and a
paragraph's indented continuation lines split off into one too.

**`X<>` index meta keeps its spaces.** Following rakudo's grammar, only the
whitespace around the `|` is skipped, so `X<t|defining, a term>` gives the
levels `"defining"` and `" a term"`. A trailing separator no longer adds an
empty level.

**`$N` after `m:g` is the N-th match.** `$/` is then a List of Matches, and
`$0`, `$1` and so on now read `$/[N]`. Before, they were the first match's
captures, or a stale `Nil` left by an earlier match.

**A quote's `}` is not a statement-ending brace.** Given
`q{<} ~ $x ~ q{>}` with `~ …` on the next line, the parser checked the whole
left operand for the "block `}` at end of line" rule. Now it checks the last
operand only, so the quote no longer ends the statement.
