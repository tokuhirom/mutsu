# X::Multi::NoMatch message: fixed duplicate `*%_` and missing arg-profile smiley

Found while fixing `signature-gist-invocant-format` (`src/runtime/class.rs`'s
`format_method_candidate_signatures`, used to build the `X::Multi::NoMatch`
"none of these signatures matches" candidate list). Two independent
divergences from raku:

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

Each candidate signature showed `Any *%_, *%_` instead of a single `*%_`.
`format_method_candidate_signatures`'s guard that skips the method's own
implicit slurpy-named param was gated on `pd.named`, but
`implicit_method_named_slurpy_param` (`method_signature_shared.rs`) sets
`named: false` on that param even though its `%_`-sigiled name makes it a
named slurpy — so the guard never fired for the implicit param and it fell
through to normal per-param rendering, producing `Any *%_` before the
explicit tail `*%_` was appended. Fixed by dropping the `pd.named &&`
condition; the name-based check (`"%_"`/`"_"`/empty) already restricts the
skip to hash-sigiled slurpies.

## Bug 2: missing `:D`/`:U` smiley on the arg-profile type

The call-argument profile (`WorkingTie.new.has-tie([1,2,3])`'s `Array`
argument) rendered as bare `Array` instead of `Array:D`.
`format_call_arg_profile` (`src/runtime/class_dispatch.rs`) now appends `:D`
for a concrete value and `:U` for a bare type object (`Package`/
`ParametricRole`), the same distinction `make_multi_no_match_error_detailed`
already used for the invocant type. Verified this does NOT apply to named
arguments — raku renders `:y(Array)`, never `:y(Array:D)` — so
`format_named_arg_profile` is untouched.

New test: `t/nomatch-candidate-signature-slurpy-and-smiley.t`.

While verifying, found a third, unrelated bug in the same function: a
genuinely *named* (non-slurpy) parameter's candidate-signature entry
renders as a positional (`Any $x` instead of `:$x!`) — out of scope here,
filed as `todo/tickets/candidate-signature-named-param-format-wrong.md`.
