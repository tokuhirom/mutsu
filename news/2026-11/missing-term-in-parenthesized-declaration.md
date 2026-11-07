# `(my $a = 42 *)` reports "Missing required term after infix"

A `my`/`our` declaration in expression position (inside parentheses, a list or
an argument list) that failed after its `my $x` swallowed the real failure and
left the generic "expected term" at the `(`, so the missing-operand diagnosis
could not see it. The declaration's own failure is now returned when it got
past the declared variable. This is the shape Text::CodeProcessing's REPL
sandbox wraps a chunk in (`my \_1 = $(⏎ chunk ⏎);`), so its `#ERR:` text now
matches raku.
