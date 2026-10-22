# Retire the `merge_all` caller-priority merge in `call_sub_value`

ADR-0055 slice 3. The tree-walk closure call (`call_sub_value`) no longer has a
`merge_all` mode: a closure's own captured binding always wins over a
same-named lexical of whatever frame calls it (dynamic variables still resolve
against the live caller chain), and the exit writeback is the single former
`merge_all == true` shape, which skips block-private names (including a
block-declared enum's bare-key alias) and values still equal to the closure's
own capture. The parameter is gone from `call_sub_value`, `vm_call_sub_value`,
`call_supply_callback` and `call_supply_tap`.

A closure created inside a `Lock.protect` block was frozen to a snapshot of a
variable the block itself wrote afterwards (`$todo++` after a `cue`), because the
per-iteration capture freeze only exempted variables the creating code *owned*.
It now also exempts free variables the creating code writes by name. The old
caller-priority merge had been hiding this; `roast/S17-lowlevel/lock.t` found it.
