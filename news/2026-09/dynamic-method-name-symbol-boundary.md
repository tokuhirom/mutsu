# Dynamic method names are interned once per dispatch

Dynamic method dispatch now interns its final method spelling once at the VM
opcode boundary and reuses that `Symbol` for native probes, override checks,
and compiled dispatch. Dynamic hyper method calls likewise share one symbol
across every target element instead of interning the same name per element.

Regression budgets pin one intern per ordinary dynamic call and zero additional
interns per dynamic-hyper target element.
