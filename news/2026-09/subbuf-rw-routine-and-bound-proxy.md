# `subbuf-rw` gets its routine form and a bound Proxy; `substr-rw`'s window follows its stores

These are the three gaps #9200 left behind (#9216).

- **`subbuf-rw` as a plain routine** was an "Undeclared routine". Only the
  assignment spelling `subbuf-rw($b, 1, 2) = ...` compiled, because it is
  lowered specially. The routine is documented core (`Type/Buf.rakudoc`), so
  it is now in the builtin list, and outside an assignment it returns the
  bound Proxy.
- **A bound `subbuf-rw`** (`my $r := $b.subbuf-rw(1, 2); $r = Buf.new(9)`) left
  `$b` unchanged without any error. The method form (`CallMethodMut` on a Buf
  receiver) and the routine form now build a write-through Proxy, the Buf
  counterpart of `substr-rw`'s. It closes over the buffer object itself, as
  rakudo's closes over `self`, and its STORE splices through the buffer's
  shared storage node, so every alias of the buffer sees the store.
- **`substr-rw`'s Proxy window** was fixed at the original `(start, len)`.
  After `$r = "ZZ"`, a second `$r = "W"` replaced only the first of the two new
  chars (`pWZab`; rakudo gives `pWab`). Rakudo's Proxy moves its char count to
  the stored string's length. The length now lives in a `ContainerRef` cell
  that both closures capture, and STORE updates it. A `subbuf-rw` Proxy keeps
  its original window, because rakudo's does too.

Both Proxy builders share one implementation, in
`src/runtime/methods_sub_rw_proxy.rs`. Pinned by
`t/regex/subst/substr-rw-subbuf-rw-proxy-window.t`.

Found on the way and filed as #9244 (not fixed here): a routine's own
`my $r := substr-rw(...)` loses its stores when a mainline `$r` is already
bound to a Proxy.
