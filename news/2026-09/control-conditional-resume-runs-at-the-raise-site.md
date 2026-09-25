# A CONTROL handler with a conditional `.resume` runs at the warning's raise site

A CONTROL handler that the compiler could not prove always resumes lost the
warnings it tried to resume (#9469):

```raku
sub inner { my $x; my $y = "a" ~ $x; say "in-after"; 3 }
sub f() {
    CONTROL { when CX::Warn { say "c"; .resume if $flag; say "not resumed" } }
    my $r = inner(); say "after $r"; 5
}
```

rakudo prints `c`, `in-after`, `after 3` and returns `5`. mutsu returned `Nil`
for an op-raised warning in the same frame, and for a warning raised in a
callee it either returned `Nil` or dropped the warning and the callee's result.

mutsu ran a CONTROL handler at the raise site only when the compiler proved it
`resume_safe`. Every other handler took the unwinding path, which can resume
only through a frame-local `resume_ip`. An opcode records no resume point, and
unwinding pops the Rust frames of every callee between the raise site and the
handler, so there was nothing left to resume into.

CONTROL now does what ADR-0072 does for CATCH. A handler whose bytecode calls
`.resume` anywhere is *resume-capable* (`OpCode::TryCatch::control_resume_capable`)
and runs inline at the raise site (`Interpreter::try_control_inline`,
`src/runtime/control_inline.rs`). Handlers are tried innermost first:

- a handler that resumes lets the raise site continue;
- a handler whose `when` arms match nothing declines, and the next outer handler
  sees the warning, still at the raise site. When every handler declines, the
  default handler prints the warning and resumes;
- a handler that matches without resuming ends its region. The raise site stamps
  the warn signal with that region's token, and the region applies the verdict
  without running the handler a second time.

The ADR-0072 amendment records the design.

Still open: a handler with no `.resume` at all (`when CX::Warn { say "x" }`)
still unwinds, so an op-raised warning in a callee under such a handler is lost
instead of reaching it. That is a separate issue.

Pin: `t/exceptions/control-conditional-resume-inline.t`.
