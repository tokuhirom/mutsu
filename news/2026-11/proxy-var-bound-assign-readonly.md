# Assigning to a variable bound to `$proxy.VAR` is a readonly-variable error

`my $b := $q.VAR; $b = 5` called the Proxy's STORE; Rakudo dies with "Cannot
assign to a readonly variable or a value" and never runs it
([#16299](https://github.com/tokuhirom/mutsu/issues/16299)), because the Proxy
object `.VAR` hands back is not a container to assign into. The local-slot store
(`exec_set_local_op`) and the by-name stores a closure reaches
(`vm_misc_assign`, the captured-lexical arm of `vm_exec_dispatch`) now raise
`RuntimeError::readonly_variable` for a slot holding a `var_object` Proxy. A
variable bound to a plain Proxy still stores through, and `my $v = $q.VAR;
$v = 5` still just replaces the held object.

Found on the way and filed separately: #16306 (`my $p := $q; $p = 7` does not
call STORE when `$q` is itself a bound Proxy variable).
Regression test: `t/vm/writeback/proxy-var-bound-assign-readonly.t`.
