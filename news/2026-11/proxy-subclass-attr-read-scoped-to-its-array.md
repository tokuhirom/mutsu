# Reading a Proxy subclass attribute no longer captures the next array mutator

Reading an attribute of a `class is Proxy` subclass remembered it so that a
following `.push`/`.pop` on the array read would write back to the shared
storage, but the memo was never cleared: the next mutating call on any array
took it, so `@rows[*-1].push: $x` died with "Cannot call 'push' on non-Array
attribute" under AttrX::Mooish. The memo now carries the identity of the array
that was read and only a mutator on that array takes it. Cooklang's nine test
files pass.
