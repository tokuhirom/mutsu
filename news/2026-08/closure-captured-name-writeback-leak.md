# Closure-exit env writeback no longer leaks captured-only names into same-named caller lexicals (90_csv 495 + end abort)

Resolved 2026-08-12. This closes the deep ticket
"Method-local reads leak a same-named closure-captured mainline lexical" —
the last real mutsu failure in Text::CSV's t/90_csv.t, which now runs all
524 tests instead of aborting after 496 (remaining failures: 159, which
rakudo fails identically, and 507-508, a separate pre-existing bug the run
never reached before — ticketed as
`todo/tickets/kh-named-array-arg-loses-container-identity-in-large-file.md`).

## The bug

In the 13-line repro (a script whose `sub sleep-time` closes over
`my $io-in = open ...`, then calls `csv (in => &getrow2)`), Text::CSV's
`method CSV` saw its own freshly-declared `my IO::Handle $io-in;` as the
TEST SCRIPT's open file handle. The forensics ticket suspected a
by-name read fallback; gdb breakpoints on the read paths proved them all
innocent — the leak was a WRITE, at closure return:

1. `sub sleep-time`'s capture boxes the mainline `$io-in` into a shared
   `ContainerRef` cell, so EVERY mainline closure's captured env carries
   `io-in => <cell>` (closures capture the whole creating scope, not just
   their own free vars).
2. Calling `$in()` (the mainline row-provider closure) from inside
   `method CSV` force-installs that cell into the callee frame at entry
   (the `ContainerRef` branch of the captured-env merge overwrites even an
   existing caller entry — it is the cell's single source of truth).
3. The exit env-writeback scan (`call_compiled_closure_with_topic`) treated
   any callee-env entry that differs from the caller's same-named entry as
   "a mutation the caller must observe". The method frame's env contained
   `io-in => Any` (its own declaration, mirrored by a smartmatch's locals
   sync), so the scan wrote the mainline handle over it, and the drain then
   pushed it into the method's local slot. Every later read of `$io-in`
   saw the script's handle — at EOF, so `getline_all` returned `[]`
   (test 495) and the next csv call died shifting an empty array (the end
   abort).

## The fix

`src/vm/vm_closure_dispatch.rs`: the exit scan now skips a captured name
that the closure body never references (not one of its free vars) and that
still holds its capture-time value — that binding is the closure's own
lexical environment, not a mutation, and the caller merely sharing the
bare NAME does not make it the same lexical. The "unchanged" test is
`values_identical` (container identity), not deep `==` — a deep eq
diverges on cyclic instances (a captured `Text::CSV.new` object sent the
first version of this fix into unbounded `Value::eq` recursion and a stack
overflow). A nested call that genuinely rebinds the name in the callee
frame still writes back.

Pin: `t/closure-captured-name-leak.t` (module-free: a class method with its
own `my $x` + a smartmatch calls a mainline closure while another mainline
sub's capture has boxed the script's `$x`).

## Container-descriptor names for `.VAR.name` (the same PR)

With the file no longer aborting, tests 505-506 exposed Text::CSV's
`@kh.VAR.name ne "element"` gate (its rakudo#2483 workaround): mutsu
reconstructed `.VAR.name` from call-site syntax, which loses the name after
one slurpy re-flatten hop. Now a `my @x`/`my %h` declaration stamps the
variable name into the fresh container's `descriptor_name`
(`SetLocalDecl` slot path + the expression-position `SetGlobal` decl path),
`:=` binds keep the original name (first name wins), and `is copy` params
reset their fresh copy to "element" (both positional and named arms — the
named arm previously did not even copy; it now detaches like the
positional one). The full rakudo-verified matrix is pinned by
`t/var-name-descriptor.t`.
