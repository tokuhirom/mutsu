# The method cascades decline a call the method table answers

The zero- and one-argument name-matching cascades now step aside for any call
a row of the built-in method table answers (ADR-11276 §9.88). Many arms left
in them only re-ran a row's own handler, so that the debug cross-check and the
VM route that honours a user's `.wrap` or `augment` would not fall into a
later, generic arm; with the decline step they are gone. The one-argument
entry also asks the table first, as the zero-argument one already did, so a
routine form such as `index($s, $needle)` and `.^can` reach the same row as
the method call.

A grammar cursor, a lazy cursor (`$¢`) and an instance of a `Match` subclass
now answer `Match`'s rows through their owner, replacing the forwarding arms
of the `Match` blocks.

The pure arity cascades went from 239 to 181 quoted-name arms.
