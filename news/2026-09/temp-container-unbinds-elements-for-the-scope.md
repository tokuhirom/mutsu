# `temp @a` detaches bound elements for the scope and reattaches them after

With `@c[0] := $x`, the block `{ temp @c; @c[0] = 5 }` wrote 5 into `$x` (#9435).
Inside a `temp @c` scope, rakudo's `@c` no longer has element 0 bound to `$x`.
At scope exit the original element container comes back, so `$x` and `@c[0]`
are one container again.

mutsu had it the other way round. The save took a copy with the bound elements
decontainerized and left the live container untouched. Inside the scope the
binding was still live, so the write reached `$x`. The restore then wrote the
decontainerized copy back, so after the scope the binding was gone for good: a
later `$x = 9` no longer showed up in `@c[0]`.

`exec_let_save_op` now snapshots the container with its element containers
intact (`snapshot_container_for_temp`) and decontainerizes the live container
in place. The restore puts the bound elements back. The container keeps its
identity throughout, so an alias (`my @alias := @c`) or a reference
(`my $r = @c`) sees the same thing as `@c`. Hashes work the same way.

Pin: `t/collections/temp-container-unbinds-elements-for-the-scope.t`.
