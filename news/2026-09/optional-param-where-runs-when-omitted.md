# An optional parameter's `where` constraint now runs when the argument is omitted

A `where` post-constraint on an optional parameter was only ever evaluated on the
branch that had a supplied argument. Omit the argument and mutsu bound the
parameter unconditionally, so a signature that exists precisely to reject certain
calls accepted every one of them:

```
$ mutsu -e 'sub f($x? where { $_ ~~ Int }) { "ran" }; say f()'
ran
$ raku  -e 'sub f($x? where { $_ ~~ Int }) { "ran" }; say f()'
Constraint type check failed in binding to parameter '$x'; expected anonymous
constraint to be met but got Any (Any)
```

Rakudo runs the constraint on every binding path. What differs is only the value
under test: an omitted optional binds its nominal type object (`Any`, or the
declared type — `Int` for `Int $x?`, and a fresh empty `Array`/`Hash` for `@x?`
and `%h?`, `Mu` inside a pointy block), and a defaulted parameter binds the
evaluated default. The constraint is then tested against *that*. Rakudo rejects
the reverse spelling `$x = 3 where { ... }` at compile time, so a defaulted
parameter only ever reaches this through `$x where { ... } = 3`.

## Why it mattered beyond the error message

A `where` clause is a multi-dispatch discriminator, so a skipped constraint does
not merely miss an error — it silently widens candidate selection.
`Digest::xxHash` is built on exactly that:

```raku
multi sub build-xxhash(Int @data, Int $seed = 0, $? where { $*KERNEL.bits == 64 } --> Int) { ... }
```

The trailing anonymous optional never looks at `$_`; it exists only so the
candidate matches on a 64-bit kernel. With the constraint skipped it matched
everywhere, on any platform.

## The fix

Both halves of the calling convention had to move, and they had to agree — if the
multi-candidate matcher and the binder disagree about a `where`, dispatch selects
a candidate that then dies binding the very call it was selected for.

- `src/runtime/types/binding_signature.rs`: the inline `where` evaluation, which
  lived only on the supplied-argument path, is now
  `check_positional_param_where_constraint`, and all three binder paths —
  supplied, defaulted, omitted-optional — funnel through it. The omitted branch
  also grew a case it never had: an anonymous optional (`$?`) binds no variable,
  so it previously fell through every branch and was never checked at all.
- `src/runtime/types/args_matching.rs`: the matcher already fell back to the
  evaluated default for an unsupplied parameter that had one, but deliberately
  skipped the check for a bare optional, on a comment asserting that rakudo
  defers it to bind time. It does not — `multi g($x, $y? where { $_ ~~ Int })`
  loses to `multi g($x, $y?)` on `g(1)` in rakudo, and mutsu now agrees. The
  named-parameter half of the matcher had the fallback right already.

## A leaked internal name, fixed on the way past

The new failures surfaced mutsu's parser placeholders in user-facing messages:
an anonymous parameter was reported as `'$__ANON_OPTIONAL__'` where rakudo says
`'<anon>'`. That was not new — a supplied bare `$ where { ... }` had been
printing `'$__ANON_STATE__'` all along. `param_display_name` now renders every
anonymous parameter as `<anon>`, sharing one `is_anonymous_param_name` predicate
with `Signature` gisting rather than keeping two copies of the placeholder list.

Pinned by `t/routines/signature/optional-param-where-runs-when-omitted.t`, which
covers all four supplied/omitted x passing/failing corners, the type object each
sigil binds, the anonymous optional and its error text, the defaulted spelling,
and four multi-dispatch cases holding the matcher and the binder to the same
answer. It passes under rakudo unchanged.
