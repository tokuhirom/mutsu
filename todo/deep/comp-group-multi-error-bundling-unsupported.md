# `X::Comp::Group` (rakudo's multi-error parse bundling) has no mutsu equivalent

## Summary

Rakudo's parser can report *multiple* simultaneous compile-time diagnoses
for a single failure and bundle them into one `X::Comp::Group` exception
(`.sorrows`/`.worries` hold the individual sub-exceptions). mutsu's parser
stops at the first fatal error and reports one diagnosis — there is no
mechanism to accumulate more than one. Two gaps in
`roast/S32-exceptions/misc.t` (found while working
`todo/tickets/vendor-real-test-module.md`) both trace to this missing
feature, not to a missing individual diagnosis:

### Case 1: `5.` (illegal decimal + malformed postfix call)

```
$ raku -e '5.'
===SORRY!===
Decimal point must be followed by digit
at -e:1
------> 5.<HERE><EOL>
Malformed postfix call
at -e:1
------> 5.<HERE><EOL>
```

Two independent diagnoses: `X::Syntax::Number::IllegalDecimal` ("Decimal
point must be followed by digit" — `5.` isn't a valid decimal literal) and
a "Malformed postfix call" (having failed as a decimal, the trailing `.`
is then tried as a method-call postfix with no method name). Roast expects
`X::Comp::Group` whose `.sorrows[0]` is the `IllegalDecimal` one:

```raku
throws-like '5.', X::Comp::Group, sorrows => sub (@s) {
    @s[0] ~~ X::Syntax::Number::IllegalDecimal
}
```

mutsu currently raises a single generic `X::Syntax::Confused` ("expected
expression statement or method name").

### Case 2: `given 42 { when SomeUndeclaredType { 1 }; default { 0 } }`

```
$ raku -e 'given 42 { when SomeUndeclaredType { 1 }; default { 0 } }'
# X::Comp::Group wrapping:
#   Function 'SomeUndeclaredType' needs parens to avoid gobbling block
#     (or perhaps it's a class that's not declared or available in this scope?)
#   Missing block (apparently claimed by 'SomeUndeclaredType')
```

`SomeUndeclaredType { 1 }` parses as a call to an unknown function/type
name with the block gobbled as an argument — again two grouped complaints.
Roast:

```raku
throws-like 'given 42 { when SomeUndeclaredType { 1 }; default { 0 } }',
    X::Comp::Group, :message(/SomeUndeclaredType/);
```

mutsu currently raises `X::Undeclared::Symbols` ("Undeclared name:
SomeUndeclaredType used at line 1") — actually a reasonable, arguably more
direct diagnosis on its own, just not the class/shape roast expects.

## Why this is deep

Both cases need the SAME prerequisite: a way for the parser to keep going
after a soft/recoverable failure, collect a second (and possibly further)
diagnosis, and bundle them into a `X::Comp::Group` value with `.sorrows`
(fatal-level) / `.worries` (warning-level) collections — not a per-case
"detect this exact input shape and throw this exact class" patch. mutsu's
parser architecture (see `docs/parser-overview.md`) is not built for
multi-error accumulation; introducing it is a parser-wide capability, not a
local fix, and needs a design pass (where does accumulation start/stop,
how do "sorrow" vs "worry" severities map to mutsu's existing
recoverable/fatal `PError` distinction, does every existing single-error
site need to keep working unchanged, etc.).

## Where this was found

`todo/tickets/vendor-real-test-module.md`'s `roast/S32-exceptions/misc.t`
gap-closing (this file's history has several rounds of individual
"typed but missing an attribute"-shaped fixes — these two are different:
they need a new *mechanism*, not a new attribute or registration).

## Suggested next step

Design a minimal `X::Comp::Group` accumulation mechanism scoped to just
enough of the parser to cover a handful of common rakudo grouped
diagnoses (illegal decimal + malformed postfix, gobbled-block function
call, and whatever else surfaces once this is picked up) rather than a
fully general multi-error parser — write an ADR if the design touches
core parser control flow (per CLAUDE.md's "Architecture decisions" policy).
