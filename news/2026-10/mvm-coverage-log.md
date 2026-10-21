# `MVM_COVERAGE_LOG` line-hit log

When `MVM_COVERAGE_LOG` names a file at process start, mutsu appends a
`HIT  <file>  <line>` record for each statement line it runs, the protocol
`Code::Coverage` speaks to MoarVM. The compiler emits one `CoverageHit` opcode
per statement line only in such a process, so a run without the variable pays
nothing. A `(file, line)` pair is written once, and a symbolic link is refused
instead of followed. The set of lines matches rakudo's for the pinned test
(#11413).
