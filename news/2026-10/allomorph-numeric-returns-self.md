# An allomorph's self-returning numeric methods answer the allomorph

`<1>.floor`, `<1.5>.abs`, `<1>.conj` and `<1e0>.Bridge` answered the bare number.
Rakudo's methods return their invocant unchanged, which is the allomorph. When a
numeric-part row hands back the very value it was given for one of those names,
`method_table::allomorph::keep_allomorph` answers the allomorph instead. A
changed value (`<-1>.abs`, `<1.5>.floor`) stays a plain number. `narrow` is
a separate wrong answer and is left alone (#13236).
