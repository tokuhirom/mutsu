# `.map`/`.grep` write-backs reach the array after a thread has run

Once a program had started any thread, `.map` and `.grep` on an Array ran on a
snapshot taken under the container stripe (ADR-0068 §16). A loop such as
`for @a.grep(* > 2) { $_ = 0 }` or `@a.grep(* > 1)>>++` then changed the snapshot,
and `@a` kept its old values, even when no other thread could see `@a`.

`.map` and `.grep` on an Array now read the live array under its stripe and
write their element promotions and `$_` write-backs back into it under the same
stripe. No user code runs while the stripe is held (ADR-0068 §16.1, #12484).
