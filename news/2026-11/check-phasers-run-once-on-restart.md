# A unit restarted from its CHECK-rewritten tree does not run its CHECK phasers again

When a CHECK phaser rewrites `$*CU`, the unit continues from the rewritten
tree. Its CHECK phasers ran a second time over that tree: Red's t/106 CHECK
saw its own grep compiled by `red-compile` and died. A CHECK phaser of the
restarted unit is now skipped; it already ran.
