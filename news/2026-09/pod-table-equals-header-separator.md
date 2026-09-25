# Pod tables: a `====|====` header separator no longer ends the table

A paragraph-form Pod table (`=for table`, or an abbreviated `=table`) whose
header separator was written with `=` characters failed to parse:

```raku
=for table
H 1 | H 2
====|====
A | B
```

mutsu reported the generic "Confused. expected statement" dump at the
separator line. The same table with a `----|----` separator worked. This kept
Pod::To::Markdown's `t/table.rakutest` from running, one of the parse gaps
split out of the #7988 cluster (#9329).

The parser skips a paragraph block by reading lines up to a blank line or the
next Pod directive, and it treated *any* line starting with `=` as the next
directive. So the table ended at `====|====`, and that line was then parsed as
code. A Pod directive is `=` followed by an identifier, and the parser already
had a helper that recognises exactly that (`parse_pod_directive_line`, used by
the `=begin` scanner). The paragraph scanner uses it now, so a line of `=`,
`+` and `|` stays part of the table. The `$=pod` builder already accepted such
separator lines, so the table comes out with rakudo's header row once it
parses.

Pinned by `t/lang/pod-table-equals-separator.t`, which also covers a mixed
`=+=` separator inside a `=begin pod` block.
