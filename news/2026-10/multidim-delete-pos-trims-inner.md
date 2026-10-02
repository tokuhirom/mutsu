# Multi-index `.DELETE-POS` trims the inner array like the single-index form

`my @d = [1,2],[3,4]; @d.DELETE-POS(0,1); say @d` printed `[[1 (Any)] [3 4]]`
(rakudo: `[[1] [3 4]]`, #10926). The multi-dimension walk (`multidim_delete_pos`)
had its own copy of the innermost delete: it left a hole but never trimmed the
trailing holes, which the single-dimension delete does.

The single-level delete is now one routine, `Interpreter::delete_pos_in_array_data`.
It marks the hole, clears `initialized` and trims trailing holes. The
single-dimension `.DELETE-POS` and the innermost step of the multi-dimension
walk both call it.
