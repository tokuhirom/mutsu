# A method call on Nil interns its probe symbols once

Before a method call on `Nil` is absorbed or refused, `nil_refused_call`
checks whether `Nil` declares rows for the method, at arities 0 through 7.
That check interned the symbols `Nil` and the method name on every arity it
tried, 16 interns per call. A grammar action reading an absent capture runs
into this every time: YAMLish's `my Tag $tag = $<properties><tag>.ast` calls
`.ast` on the `Nil` that `$<properties><tag>` answers, about 240 times per
`load-yaml` of `bench-yaml-parse-big`'s document. The two symbols are now
interned once per call (#12917).
