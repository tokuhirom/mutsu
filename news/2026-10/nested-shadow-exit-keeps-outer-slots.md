# A nested shadow's block exit no longer clobbers further-out bindings

`my $x = 1; { my $x = 2; { my $x = 3 }; say $OUTER::x }` printed `2` instead of `1`.
When a block that declares `my $x` exits, `exec_block_scope_op` restores the slot the
block's declaration hid, taking the value from the name-keyed `restored_env`. It wrote
that one value into *every* same-named slot the block did not own, so the innermost
block's exit also wrote the middle block's `2` into the outermost `$x` slot. That slot
was repaired only when the middle block exited, and until then `$OUTER::x` (which reads
the slot directly under shadow slots) saw the dead inner value.

The compiler already knows which slot each shadowed name denotes after the block: it is
the `prev` entry in the block's local-scope frame. Every ancestor frame that also
shadowed the name recorded a further-out live slot. A statement-position `BlockScope`
now records those further-out slots in `CompiledCode::block_scope_protected_slots`, and
the exit leaves them alone. The restore itself stays, because it still repairs by-name
writebacks that land in the immediately enclosing slot.

Regression test: `t/vm/scope/nested-shadow-exit-keeps-outer-slots.t` (#10856).
