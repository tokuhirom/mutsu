# `$*STACK-ID`

`$*STACK-ID` (Rakudo 2022.06+) identifies the call stack that is running:
the mainline is 0, and every `start` block, pooled task and `Thread` body is
a stack of its own with a fresh id. Unlike `$*THREAD.id`, two tasks that one
pool worker runs back to back do not share an id. Log::Dispatch uses it to
tag each log line with the stack that wrote it; with it, Log::Dispatch's test
suite passes (`t/040-threaded` died with "Dynamic variable $*STACK-ID not
found").
