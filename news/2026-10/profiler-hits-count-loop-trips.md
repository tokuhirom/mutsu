# Profiler `hits` counts every loop trip, and a call's return no longer counts twice

The line profiler's exact `hits` column (ADR-0106 Slice 3) counted line
*transitions*: a line was counted each time control arrived from a different
line. A loop whose body sits on one line never transitions, so
`for 1..100 { $a = $a + 1 }` — and every other loop form, including a body on its
own single line — reported `hits 1` next to a line carrying most of the run
(#8737).

A hit is now an *arrival* at a line, and coming back around a loop is an
arrival even on the same line: the poll network ends the current line visit
when a compound loop enters its body for the next trip, or when control takes a
backward jump (`nqp::while`), in the interpreter and in native code alike. A
line holding a whole loop reports trips + 1 (reached once, then once per
trip), which is what a multi-line `while`'s header line already reported.
Nothing changes when the profiler is disarmed: the arrival flag is read only
inside the armed-profiler branch of `vm_poll::poll_code`.

Measuring JIT parity on the new fixtures turned up a second, older gap. The
interpreter polls every op under the profiler, so after a call returned it saw
the calling line "arrive" again and counted it; native code hooks only line
transitions and jump targets and did not. A line that makes a call therefore
reported different `hits` with the JIT on and off (21 against 20 for a
20-iteration loop), breaking ADR-0106 §8 gate 4. The counter's "current line"
is now kept per routine depth, so a callee's lines leave the caller's alone and
a return is not an arrival — both modes agree, and `docs/profiler.md` no longer
has to warn that a calling line is entered twice.

`tests/profile_counts.rs` pins one-line `for`, `while`, `loop`, `repeat`,
statement-modifier `for` and `nqp::while` at trips + 1 with JIT parity, and a
calling line at one hit per arrival. A remaining gate-4 gap — an interpreted
routine body on a call fast path records no line hits at all — is #11660.
