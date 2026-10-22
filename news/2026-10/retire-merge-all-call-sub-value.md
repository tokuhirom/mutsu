# Retire the `merge_all` caller-priority merge in `call_sub_value`

ADR-0055 slice 3. The tree-walk closure call (`call_sub_value`) no longer has a
`merge_all` mode: a closure's own captured binding always wins over a
same-named lexical of whatever frame calls it (dynamic variables still resolve
against the live caller chain), and the exit writeback is the single former
`merge_all == true` shape, which skips block-private names (including a
block-declared enum's bare-key alias) and values still equal to the closure's
own capture. The parameter is gone from `call_sub_value`, `vm_call_sub_value`,
`call_supply_callback` and `call_supply_tap`.
