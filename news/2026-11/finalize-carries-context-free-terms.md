# The executing finalization keeps context-free terms as the parser built them

A census of the constructs `finalize_unit` converted to RakuAST and lowered back
on their own (the "fallback" of ADR-15760 section 2.6) over 7000 `t/` files showed that
most of them lowered to the identical tree: `self`, `*`, `$/`, `$!` and attribute
variables (`$!x`) accounted for about a third of the converted expressions.
Those are keyword-like or special terms that nothing the unit declares can
reclassify, so they are now carried as the parser built them (`carried::expr_carried`),
like a plain variable already was. The remaining fallbacks by size are regex
literals with code, `WhateverCurry`, subscript adverbs, `Supply`, native type
barewords (`uint8`, `int32`, ...) and `callsame`-like barewords, which need the
unit's scope to classify; they are the next families to lower where the parser
builds them.
