# An augmented `multi method` with only an invocant outranks the built-in

`augment class Str { multi method FatRat(Str:D:) { ... } }` was ignored: the
implicit `*%_` every method carries made the candidate look "slurpy-only",
which ranks after the built-in method (#12751). Only a positional slurpy or
capture does that now, so FatRatStr's `'1.66e-27'.FatRat` reaches the module's
exact parser and FatRatStr's four test files agree with Rakudo.
