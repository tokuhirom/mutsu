# Lvalue-method writeback no longer clobbers the caller's same-named lexical

`$state.top = 5` inside a sub compiles to an lvalue builtin that writes its
target back *by name*. When `$state` was a file-scope lexical the sub
captured, that name's env key belonged to whichever frame was calling the sub
(the frame env is rooted at the live caller), so a caller with its own
unrelated `my $state` had that variable replaced by the sub's object. Plain
reads and stores already go to the compunit cell first; the lvalue builtins
(`$x.attr = v`, `$x.substr-rw(..) = v`, `$x.value = v`, `@x[0].attr = v`)
did not.

The VM now binds such a target, for the duration of the builtin call only, to
the routine's own compunit cell in the frame's env tier, before the
raw-invocant box resolves the invocant's container by the same name; the
tier's previous entry is restored afterwards. A target that is a local of the
running frame is never redirected.

This was what broke Terminal::UI's `t/09-print.rakutest`: `Terminal::ANSI`'s
module-level `$state.scroll-top = ...` replaced the `my $state` of
`Terminal::ANSIParser`'s parser closure (#11275).
