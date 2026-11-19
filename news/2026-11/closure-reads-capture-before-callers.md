# A closure resolves a lexical through its capture before its callers

A closure written in a routine and called from a nested block of that routine read the block's
same-named `my` instead of its own outer one: `my $X = "outer"; sub d() { my $c = { $X };
{ my $X = "block"; $c() } }` gave `block`, and a `my class Inner` in the block likewise answered
`Inner` inside the closure (#18175). The closure's frame env is chained over its caller's, so the
by-name lookup met the caller's block before it reached the closure's capture.

A closure's frame root is now marked as resolving through its outer (ADR-12529 phase 3,
`Env::mark_outer_is_capture`): a lexically scoped name that misses the frame answers from the
capture -- the scope the closure was created in -- and then from the live program scope, before
any caller frame. Dynamic variables still walk the callers, and a name neither binds still does
too, because the capture is a creation-time snapshot: a code variable its scope declares after
the closure (`my &a = { b() }; my &b = ...`) is still found where the creating frame binds it.
That last fallback goes once a closure's outer is a live scope handle (phase 4).

Two consequences were fixed with it. A closure's capture no longer binds a name to a slot the
creating frame had not declared at the creation point (a later block's `my $X`, which read as
`Nil`); #18176 had done this for sigilless terms only. And a type check no longer re-resolves the
*argument's* registered type name in the running scope, so a package class `Inner` handed to a
closure whose parameter names a block's `my class Inner` is rejected, as in rakudo.
