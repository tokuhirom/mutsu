# A Proxy bound to `PROCESS::<$X>` through a variable stays assignable

`PROCESS::<$P> := $p` with `$p` a `Proxy` marked `$*P` as an immutable binding (the "no container"
mark the previous slice added for `PROCESS::<$X> := value`), so `$*P = 'a'` died with `Cannot modify
an immutable Package`. The bound value reaches the stash as the variable's container, so the Proxy
check now looks through it (`src/vm/vm_var_assign_index_named.rs`). Test:
`t/vm/scope/process-dynamic-bind-no-scalar.t`. #16813 (the Proxy's STORE writing a copy of the creating
frame's lexicals) is still open.
