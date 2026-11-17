# A resumed CONTROL handler's env writes are counted, not logged

A resume-safe `CONTROL` handler run inline at a warn raise site writes the
installing frame's lexicals with no call opcode in between, so a leaf closure's
return path has to know such a write happened or it skips its caller-writeback
scan. `Interpreter::inline_control_env_writes` recorded each written name in a
`Vec` that was never cleared — one entry per resumed warning, for the life of
the program — while its readers only ever compared its length with the length
at closure entry.

It is now `ControlState::inline_env_write_gen`, a counter the CONTROL handler
bumps and each closure call compares with its value at entry. ADR-10779 D3:
`Interpreter` loses a field to the control subsystem, and a long-running
program that resumes warnings no longer grows a list it never reads.
