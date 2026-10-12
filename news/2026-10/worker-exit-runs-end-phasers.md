# A worker thread's `exit` runs the END phasers

`END { say "end ran" }; start { exit 3 }; sleep 1` now prints `end ran` and exits 3, as Rakudo
does. A sunk `start` that dies now runs END too. Before this change, the worker ended the
process directly and skipped END (#11617).

ADR-11617 hands the worker's `exit` over to the main program's interpreter. That interpreter
unwinds its mainline as if it had called `exit` itself, runs END, and exits with the worker's
status. It reacts from a busy loop or from a `sleep`, an `await` or a `Channel.receive`. If the
main thread is stuck in native code and does not adopt the `exit` within a second, the worker
ends the process itself, as before.
