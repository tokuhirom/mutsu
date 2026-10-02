# Methods share one cell with an enclosing `my &k`

A class or role method that assigned an enclosing `my &k` (`&clock = &new`)
read a stale copy once the declaring scope reassigned it: `SetGlobal("&clock")`
put `&clock` in the method's free variables, so the declared-method capture
snapshotted the slot, but `box_decl_local_cell` skips `&` locals, so the
snapshot was a plain value rather than a shared cell. A method that only *read*
`&k` had the opposite problem: the scope-blind method compiler never received
`outer_code_var_names`, so the `GetCodeVar("k")` read was not a free variable
at all and resolved against whichever frame called the method.

The method compiler now inherits `outer_code_var_names` like a nested named
sub, and the declared-method capture boxes `&` slots with
`box_decl_local_cell_any_sigil` (the cell `#11051` introduced for escaping
`our sub`s), so the method, its siblings and the declaring scope all read and
write one container (#11046; found via MVC::Keayl's `Job.clock`).
