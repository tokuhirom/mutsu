# Stringifying a list now runs a nested un-run `.map` Seq

A `.map`, `.grep` or `gather` Seq nested inside a list does not run its
callback until something reads it. `.Str`, `.join` and string interpolation
of the outer list rendered such an element through the pure renderer, which
saw only the empty seed, so `((3,4).map({$_}),).Str` was `""` and
Pod::To::HTML's tables rendered every body row as `<tr>  </tr>`.

The list-stringify probe (`list_str_needs_interpreter`) and the `.join`
probe (`str_needs_dispatch`) now flag a nested un-run Seq, and the resolver
reifies it in place before rendering. Pod::To::Markdown's `t/table.rakutest`
and Pod::To::HTML's `t/060-table.rakutest` now pass.
