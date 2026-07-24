# A slurpy hash is not a positional catch-all

`sub f(Str $a, Str $b = '', *%o)` accepts at most two positional arguments — the
`*%o` collects *named* ones. mutsu's dispatch type-check treated it as a
positional slurpy, which set `has_variadic_positional` and switched off the
"too many positionals" rejection entirely, so such a candidate matched a call
with any number of positionals. Binding then failed with the
`Calling f(Str, Str, Str, Str) will never work with declared signature` error
that resolution should have avoided by not picking the candidate at all.

A slurpy `%`-sigil parameter is now excluded from the positional parameter set
in `args_match_param_types`.

This surfaced while chasing `OpenSSL::CryptTools`, whose `encrypt` chain relies
on a named argument written before a positional. That larger fix is blocked on
PLAN.md §8.22 — a `unit module`'s routines register under `GLOBAL`, so
`Test::Util`'s non-exported `our sub run(Str $code, Str $input = '', *%o)` is a
live candidate for the builtin `run` in every file that `use`s `Test::Util`.
This half landed on its own because it is independently correct.

Pinned by `t/named-slurpy-not-positional.t`, which passes unchanged under `raku`.
