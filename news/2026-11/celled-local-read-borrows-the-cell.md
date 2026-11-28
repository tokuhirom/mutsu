# Reading a `:=`-bound variable no longer clones its cell handle

A variable bound with `:=` (`my @alias := @data; my $sref := $seed`) or captured by a closure
holds a binding cell in its slot. `GetLocal`'s fast paths cloned that cell's handle out of the slot
and then read through the clone, so every read paid a refcount increment and decrement on the
handle and, on the handle's drop, the cycle collector's acyclic check.

The fast paths now read through the slot's own handle with `Value::deref_clone`, which clones only
the cell's value, exactly the value the owned path produced. Container mode, which may hand out the
cell itself, keeps the owned path.

Measured with callgrind (#19291): `bench-threads-serial`'s read loop (`@alias[$i +& 255] + $sref`)
costs ~394 fewer instructions per iteration, and a tenth-size `bench-threads-serial` runs 8.7%
fewer instructions.
