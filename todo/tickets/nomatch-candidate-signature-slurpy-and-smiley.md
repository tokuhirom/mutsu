# X::Multi::NoMatch message: duplicate `*%_` in candidate signatures and missing `:D`/`:U` smiley on the argument-profile type

Found in passing while fixing `signature-gist-invocant-format` (`src/runtime/class.rs`'s
`format_method_candidate_signatures`, used to build the `X::Multi::NoMatch` "none of these
signatures matches" candidate list). Two independent divergences from raku:

```raku
class WorkingTie {
    multi method has-tie(Int $z) { }
    multi method has-tie(Str $z) { }
}
WorkingTie.new.has-tie([1, 2, 3]);
```

```
raku:  Cannot resolve caller has_tie(WorkingTie:D: Array:D); none of these signatures matches:
           (WorkingTie $:: Int $z, *%_)
           (WorkingTie $:: Str $z, *%_)
mutsu: Cannot resolve caller has_tie(WorkingTie:D: Array); none of these signatures matches:
           (WorkingTie $:: Int $z, Any *%_, *%_)
           (WorkingTie $:: Str $z, Any *%_, *%_)
```

## Bug 1: duplicate `*%_`

Each candidate signature shows `Any *%_, *%_` instead of a single `*%_`. `format_method_
candidate_signatures` (`src/runtime/class.rs:298-360`) is supposed to skip the method's own
implicit slurpy-named param (the `if pd.named && (pd.slurpy || pd.double_slurpy) && (pd.name ==
"%_" || pd.name == "_" || pd.name.is_empty())` guard at :326-331) and append `*%_)` itself at
the end — but the guard's name check doesn't match whatever name the implicit slurpy param
actually carries for a method (unlike the `_capture`/`__ANON_*` sentinel names the guard was
written against elsewhere), so it falls through to the normal per-param rendering and produces
`Any *%_` before the explicit tail `*%_` is appended.

## Bug 2: missing `:D`/`:U` smiley on the arg-profile type

The call-argument profile (`WorkingTie.new.has-tie([1,2,3])`'s `Array` argument) renders as bare
`Array` instead of `Array:D`. `format_call_arg_profile` (sibling helper, same call site in
`src/runtime/class_dispatch.rs`) needs to append the smiley the way `invocant_concrete` already
does for the invocant type in `make_multi_no_match_error_detailed`.

## Scope note

Both bugs are message-cosmetic only (dispatch itself already correctly reports no-match); they
were explicitly left out of `signature-gist-invocant-format`'s fix, which only touched the
invocant-marker portion of the same message (`(WorkingTie $:: ...)` vs `(WorkingTie: ...)`).
