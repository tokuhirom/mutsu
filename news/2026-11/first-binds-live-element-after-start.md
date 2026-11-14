# `.first` answers the live element after a `start`

`.first` on an Array answers the matching element's own container:
`my $x := @a.first(* > 1); $x = 9` writes `@a`, as on rakudo. Once the program
had started any second thread, though, `.first` ran on a snapshot copy of the
array (the ADR-0068 §20 read rule), so the binding wrote the copy and the
assignment vanished.

Red's CHECK-time compiler rewrites every `.first` call through such a binding.
A model's `create-table` emits its event from a `start` block, and after that
`@values.first(* > 1) = 20` left the array unchanged (Red t/091, "Native
writable return values").

`.first` on a non-shaped Array is now exempt from the snapshot, as `.map` and
`.grep` already were (ADR-0068 §16.1). Its chunked scan reads the length and
promotes each chunk under the container stripe, and runs the matcher on the
copied-out chunk with nothing held.
