# A Method object called with a captured variable keeps its invocant

`my $m = $obj.^find_method("who"); sub k(|c) { $m(|c) }; k($obj)` called the method with `self`
unset: a variable spread through a capture arrives as its container, and the `CALL-ME` of a
Method object tested the container, not the value, against the owner class before re-entering
method dispatch. `CALL-ME` now decontainerizes the invocant first
(`src/runtime/methods_instance_ops.rs`). Found by JSON::RPC's `t/01-server.rakutest`, whose
`!call(Method $candidate, |params)` does `$candidate(|params)`. Test:
`t/oo/method/method-object-call-capture-invocant.t`.
