# `my ($a, @b) = ... if COND` declares its variables unconditionally

A declarator list with an `if`/`unless` modifier used to leave its variables
undeclared when the condition was false (`Variable '@wb' is not declared`).
`try_split_decl_modifier` now splits a list declaration of plain `$`/`@`/`%`
elements the way it already split the single-variable form: the declaration
always runs, only the list assignment is gated (#12385). A `:=` list is gated as
an assignment for now (marked with a TODO).
