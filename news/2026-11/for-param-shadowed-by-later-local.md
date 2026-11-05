# A `for ... -> $h` parameter is not clobbered by a later `$h` of the unit

`for $a.list -> $h { $h<x> = 1 }` stored into the first element only when the same unit declared
another `$h` later (`for 1, 2 -> $j, $h { }`, `my $h`, ...). The loop parameter had no slot when the
loop was compiled, so its body used by-name accesses, but by run time the later declaration had
given the code object a slot of that name, which by-name lookups prefer: the second iteration read
the first element through the stale slot. The finalize pass of a chunk now names such a slot as the
loop's `param_local` (`CompiledCode::bind_for_params_to_later_locals`), so the loop keeps it in step
with the parameter and restores it afterwards, as it already did for an earlier declaration. Found
by JSON::RPC's `t/00-client.rakutest`. Test: `t/routines/signature/for-param-shadowed-by-later-local.t`.
