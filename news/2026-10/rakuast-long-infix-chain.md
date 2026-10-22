# RakuAST: long infix chains are bounded by stack headroom

Two walks over a RakuAST tree (user-subclass normalization before lowering, and
the placeholder-declaration scan) refused a tree after a small fixed nesting
count (512 and 128), so a 300-operand `~` chain such as the huge-Int literal in
`roast/S32-num/int.t` failed under `MUTSU_RAKUAST=1` with "Too deep recursion".
On a thread with a native stack floor they are now bounded by the real stack
headroom (`native_stack::headroom_exhausted`), as the VM's call guard is; a
thread without one keeps the old fixed bound. `roast/S32-num/int.t` passes under
the RakuAST frontend.
