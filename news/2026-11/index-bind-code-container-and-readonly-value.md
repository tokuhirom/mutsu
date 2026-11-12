Indexed `:=` now shares an assigned `&code` variable's container, so writes
through the element update the variable. Binding a bare Block, object, or
computed aggregate without a source container makes the element read-only, as
`BIND-POS` and `BIND-KEY` do.
