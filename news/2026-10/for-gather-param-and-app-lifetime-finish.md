# Selkie screen teardown: a gather-sourced `for` restores its parameter, and `.finish` joins an app_lifetime thread

Two independent gaps behind Selkie `t/105-screen-subscription-lifecycle` (#15208):

- A `for SOURCE -> $x` over a `gather` (the lazy path, `exec_for_loop_lazy_gather_from`) never
  saved and restored the loop parameter's prior binding, as the eager path does. The last item
  leaked through the frame merge into a caller with a same-named binding: `Widget.set-store`'s
  `for self.children -> $child` over `CardList.children` (a `gather`) replaced
  `Container.add`'s own `$child`, so the container stored the card's border instead of the
  `CardList` and `unsubscribe-subtree` never reached it.
- `Thread.start(:app_lifetime, ...)` dropped its join handle, so `.finish` returned at once.
  `Selkie::Trace`'s writer thread therefore never got to write an event before `shutdown`
  read the file back. The handle is kept apart (it is still not waited for at exit), and
  `.finish` joins it.
