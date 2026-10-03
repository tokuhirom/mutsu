# Proxy FETCH is an O(1) closure call

Reading a `Proxy`-bound variable ran its FETCH through the interpreter's
`call_sub_value` carrier in `merge_all` mode. That mode builds a copy of the
whole caller env for every call, so a read cost time linear in the number of
locals in the reading frame. A FETCH in a frame with 1000 locals was about 30
times slower than calling the same block directly (#9385).

`auto_fetch_proxy` now calls FETCH through the VM's value-call path
(`vm_call_on_value`), the same path as `c(1)`. The block runs against its own
captures. A lexical that the STORE side mutates is a shared cell, so FETCH still
sees its current value. Dynamic variables still resolve through the caller
chain. Env effects are still thrown away after the read. With the issue's
repro, 5000 reads take 0.017s at L=250 and 0.018s at L=1000. Before the change
they took 0.175s and 0.441s. The `~~ Proxy RHS vs frame locals` case in
`scripts/vm-complexity-check.sh` is now flat.

The switch exposed a separate bug. An anonymous `method` called through its
code value (`$m(self)`) took its private-method caller from whatever method
called it, not from the package it was declared in. The interpreter carrier
had pushed such a call as a block frame, which hid the bug. Only a direct VM
call showed it. `private_calling_package` now treats an anonymous routine
frame like a closure block, so the package the code was written in wins.
