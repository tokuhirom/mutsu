# A closure captures the routine's `&infix:<op>` lexical

A closure that names the enclosing routine's operator-named code lexical
(`sub mk(&infix:<cmp>) { * cmp * == More }`, or an explicit
`&infix:<cmp>($x, $y)`) now captures that binding, so called after `mk` has
returned it still uses the comparator that was passed in. The free-variable
scan skipped every `&name` containing `:`, which left operator-named
lexicals to be resolved by name against the *calling* frame at run time (the
builtin operator, once the routine had returned). BinaryHeap's
`heapsort({$^b cmp $^a}, @array)` builds exactly such a closure
(`GenHeap[* cmp * == More]`); its ordering now matches rakudo. The remaining
in-place difference is #16988.
