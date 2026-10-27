# A user set-operator candidate is called with the operands themselves

`multi infix:<(|)>(Red::ResultSeq $a, Red::ResultSeq $b)` (and `∪ (&) ∩ (-) ∖ (^) ⊖ (+) ⊎ (.) ⊍`) never ran for
`Iterable` operands: the six set-operator opcodes read an `Iterable` instance as its
element list before they asked the user override, so the candidate's typed
parameters saw an Array and the built-in answered a `Set` of the rows. The
override is now asked first, with the operands as written; the built-in walk of
the operands follows only when no candidate takes them. Red's
`M.^all.grep(...) (|) M.^all.grep(...)` returns the combined `ResultSeq`
(`t/14-union`).
