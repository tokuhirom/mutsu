# Reading a gather one element at a time is linear

A consumer that pulls a `gather` Seq one element at a time — a subscript in a
loop (`$g[$_] for ^$n`), a lazy pipe stage or adaptor whose source is a gather —
used to copy the gather's whole reified prefix on every pull and store it as a
new generation of the Seq's body, so `n` reads cost O(n²) time and kept O(n²)
elements alive until the Seq was dropped.

The prefix pull now asks the coroutine only for the elements the body does not
hold yet and appends them to the body's generation store, whose buffers grow
geometrically (#13023). `n` single-element reads copy O(n) elements in total.
This removes the blocker ADR-11678 §8.5 recorded for moving the remaining
streaming consumers of a gather onto its Seq body (#13026).
