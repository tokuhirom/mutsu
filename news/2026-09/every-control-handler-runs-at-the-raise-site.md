# Every CONTROL handler runs at the warning's raise site

A CONTROL handler with no `.resume` in it never saw an op-raised warning
from a callee (#9510):

```raku
sub inner { my $x; my $y = "a" ~ $x; say "in-after"; 3 }
sub o() { { CONTROL { when CX::Warn { say "inner saw" } }; inner(); }; say "o after"; 1 }
say o();
```

rakudo prints `inner saw`, `o after`, `1`. mutsu printed only `o after`, `1`:
the handler never ran and the warning was lost.

The #9469 fix ran a CONTROL handler inline at the raise site only when its
bytecode contained `.resume`. Any other handler still unwound. The unwinding
signal for an op-raised warning carries its resume value in `return_value`,
and the call boundary read that as an explicit `return`. So `inner` just
returned, and the signal never reached the region. A handler whose `when`
arms matched nothing had a second problem: unwinding had already dropped the
callee, so the default handler could not print the warning and resume it.

Now every CONTROL handler runs at the raise site, as rakudo runs it on top of
the stack. The `control_resume_capable` opcode flag and the unwinding fallback
in `try_control_inline` are gone. A handler that matches without resuming
still ends its region through the `(token, Handled)` stamp. When every handler
declines, the default handler prints the warning and the callee continues.

This had been held back by cost. Carrying the handler's bytecode deep-cloned
the enclosing `CompiledCode` and `CompiledFns` on every region entry (#9172).
Now the copy is taken once and shared. `CompiledCode::shared_snapshot` caches
one `Arc` per code object, and `Interpreter::shared_fns_snapshot` caches one per
function-table version (`CompiledFns::id`). The resume-capable CATCH entries
(ADR-0072) use the same caches, so their per-entry clone is gone too.

A warning raised in the handler's own frame now runs inline as well. The
handler writes that frame's locals through env. When it ends the region, no
call boundary copies those writes back into the live slots, so the region does
that itself when it applies the `Handled` stamp.

`t/exceptions/control-handler-without-resume-inline.t` pins it. ADR-0072 has an
amendment recording the decision.
