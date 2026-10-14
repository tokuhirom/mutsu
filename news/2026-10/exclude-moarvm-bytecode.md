# MoarVM::Bytecode recorded as an ecosystem dead end

Worked the ecosystem queue entry MoarVM::Bytecode. Its suite loads `CORE.c.setting.moarvm` from
`$*EXECUTABLE.parent(3)`, i.e. the install tree of a MoarVM-based compiler, which mutsu does not
have. The cause is filed as #13511 and the distribution is added to `ecosystem/exclude.txt`, so
the queue does not offer it again.
