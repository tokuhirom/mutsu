# `$*STACK-ID` names the running call stack

`$*STACK-ID` died with "Dynamic variable $*STACK-ID not found". Rakudo has had
it since 2022.06, and Log::Dispatch uses it for its `:thread-id` once the
compiler version is at least that (#11269).

It now reads `0` in the mainline and a fresh Int on every other stack. Every
task the worker pool runs (a `start` block and the other pooled tasks) and
every `.then`/`.andthen`/`.orelse` callback gets a new id for its duration,
even when one worker runs several of them in turn. A thread started any other
way draws an id on its first read and keeps it. Like the other lazily built
magic variables, it is answered on an env miss, so a `my $*STACK-ID` still
shadows it.
