# A `|c` closure binds the caller's variables, not the program scope's

Since #18237 a closure frame resolves a lexical through its capture and the
program scope before its callers. The closure dispatch ran its argument binder
with that lookup active, so a `-> |c { ... }` closure looking up the call
site's argument names (to make the capture's elements the caller's
containers) found a same-named variable of the program scope first. A module
method's `my $e` handed to such a closure arrived as the test script's own
`$e` whenever the method was itself called from a closure.

NativeCall installs exactly such a closure as a native sub's body, so
Gnome::Glib's `t/Error.rakutest` regressed in sweep #109 with "Native call
expected argument 1 with CArray representation, but got a P6opaque". The
closure binder now runs with static links suppressed, as a named routine's
binder already did; parameter defaults still follow the frame's static link
(#18253).
