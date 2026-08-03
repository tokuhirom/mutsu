# Named parameters do not narrow a multi candidate

`Digest::HMAC`'s dependency declares three candidates that differ only in which
of two *named* parameters carries a type:

```raku
multi hmac(Str :$key,      :$msg, :&hash, :$block-size) { samewith key => $key.encode, … }
multi hmac(    :$key, Str :$msg, :&hash, :$block-size) { samewith :$key, msg => $msg.encode, … }
multi hmac(Blob :$key is copy, Blob :$msg, :&hash, :$block-size) { … }
```

Called with both `:$key` and `:$msg` as `Str`, mutsu answered
`Ambiguous call to 'hmac()'`. Rakudo runs candidate 1, whose `samewith` reaches
candidate 2, whose `samewith` reaches candidate 3.

## Narrowness is a property of the positional parameters

Raku computes candidate narrowness from the **positional** parameters. A named
parameter's type decides whether a candidate is *applicable*; it never makes one
candidate narrower than another. Two candidates that differ only in their named
types therefore land in the same narrowness group, and rakudo resolves that by
**declaration order** rather than reporting ambiguity. Verified directly:

```raku
proto p(:$a) {*}
multi p(Any :$a) { "Any" }
multi p(Int :$a) { "Int" }
say p(a => 1);       # rakudo: Any  -- Int does NOT outrank Any
```

Reversing the two declarations makes the same call answer `Int`. The positional
analogue is genuinely ambiguous in rakudo, and stays ambiguous here:

```raku
multi g(Str $a,     $b) { }
multi g(    $a, Str $b) { }
g("x", "y")          # X::Multi::Ambiguous, in rakudo and in mutsu
```

And a positional still narrows even when a named "disagrees":
`multi r(Any $x, Int :$a)` / `multi r(Int $x, Any :$a)` resolves `r(1, a => 1)`
to the `Int $x` candidate in both implementations.

## The change

Three places in `dispatch_candidates.rs` stopped consulting named parameters:

* `candidate_specificity_rank_for_args` computes its type-narrowness components
  (`typed_param_count`, `subset_type_count`, `literal_value_count`,
  `where_count`, `subsig_count`, `trait_count`) over positionals only. How
  *many* nameds a candidate declares stays a tie-break — a signature that
  accepts more of the call's nameds is still the better fit — only their types
  are excluded.
* `candidate_type_distance` skips named parameters, so a named type cannot move
  the secondary sort key either.
* The ambiguity report is suppressed when any tied candidate has a
  type-constrained named parameter (`candidate_has_typed_named_param`). That
  mirrors rakudo's own mechanism: such a candidate needs a trial bind, and the
  first one that binds wins, so the tie never reaches the ambiguity check.

`hmac(key => "Jefe", msg => "what do ya want for nothing?", hash => &sha1,
block-size => 64)` now produces the RFC 2202 vector
`effcdf6ae5eb2fa2d27416d5f184df9c259a7c79`. Pinned by
`t/multi-named-narrowness.t`, which passes under rakudo as well as mutsu.

## Known residue

When candidates tie, mutsu takes the first in the *sorted candidate list*, and
`sort_candidates_by_specificity` breaks an equal-rank tie on the registry key
string rather than on declaration order — the registry is a hash map, so
declaration order is not otherwise preserved. That is right often enough for the
cases above, but `multi h(:$a)` declared before `multi h(Str :$a)` still picks
the typed one where rakudo picks the untyped. `FunctionDef` already has a
`decl_order` field (stamped only for proto-token candidates today); extending it
to every multi is the fix, and is recorded in
`todo/tickets/multi-tie-break-declaration-order.md`.
