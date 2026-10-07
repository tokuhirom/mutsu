# A closure called from native code reads dynamic variables live

A closure run through `call_sub_value` (for example an `Attribute.set_build`
callback invoked by `.new`) persisted its whole captured env after the first
call, dynamic variables included, and then let that snapshot overwrite the
caller's live binding. `%*ENV` read in such a closure stayed at the first call's
value once the class had been declared after an earlier `temp %*ENV` block.
Dynamic variables are now never persisted and never override the live caller
chain, matching the VM closure path (#12279).
