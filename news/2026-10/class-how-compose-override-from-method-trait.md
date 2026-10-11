# A `compose` override mixed into a class HOW by a method trait is driven

A method trait in a class body can mix a role with a `compose` method into the
class's HOW (`$*PACKAGE.HOW does R`, as `Method::Also`'s `is also<alias>` does).
The class declaration took its "had a compose hook" snapshot after the body had
already run, so such a hook was never queued. The snapshot is now taken before
the body, class composition drives the override, and the `is also` alias special
case in `registration_class_body_method.rs` is gone. `Method::Also`'s
`t/01-basic.rakutest` stays at 11/11 and matches `raku`.

Closes #12747.
