# Pseudo-package values compare by type-object identity

The `=:=` compiler path now treats `::?CLASS`, `::?ROLE`, `::?PACKAGE`, and
`::?MODULE` as bare package values. Comparing a class term with its own
`::?CLASS` therefore succeeds, while a scalar copy of that type object still
has its own container and compares unequal to the bare value.
