# Invoking a user class as a coercion (`Foo($x)`) works

Invoking a type object coerces: `Foo($x)` is `Foo.COERCE($x)` when the type
defines `COERCE`, else `Foo.new($x)`, else `X::Coerce::Impossible`. mutsu already
took that path for built-in types (`Int("42")`), for roles, and for enums — but a
**user-declared class** had no branch at all, so

```raku
class Locale::Dates { multi method new($locale = "EN") { … } }
my $ld = Locale::Dates("DE");
```

died with `Unknown function: Dates`: the call fell through every routine lookup,
and the qualified name collapsed to its last component before being reported.

`call_function_fallback` now handles a class type object with the same protocol
the role branch implements after punning, in raku's precedence order:

1. `CALL-ME` — a type object carrying one is *invocable*, not coercive;
2. `COERCE`;
3. `new`;
4. otherwise `X::Coerce::Impossible`, with raku's wording
   (`Impossible coercion from 'Str' into 'B': no acceptable coercion method
   found`).

A coercion takes **one** value: `B("q", "r")` coerces the `List`, it does not
splat two arguments. raku shows this by accepting that call for
`method new($x)` (which receives the List) and rejecting it for
`method new($x, $y)`. mutsu now wraps multi-argument calls the same way.

A `COERCE` with **no matching candidate falls back to `new`**, which is what raku
does — a class may declare `multi method COERCE(Str)` alongside
`multi method new(Int)` and accept both spellings. This mirrors what the role
branch already did.

## Two traps this fix walked into first

Both were caught by running the suites before committing, and both are pinned:

1. **The class branch must not shadow the role branch.** Coercing a role *puns*
   it to a class, so after the first `R("x")` the pun makes `has_class("R")` true
   and a class-first branch takes over every later call — with the pun's `COERCE`
   found but not its `new`, so `R(42)` died with "No matching candidates for
   method: COERCE". This broke the whitelisted
   `roast/S12-coercion/coercion-methods.t` ("Roles" subtest). The class branch is
   now gated on `!has_role(name)`.
2. **Arguments are not splatted** (above). The first implementation passed them
   through, which silently accepted `B("q","r")` for `method new($x, $y)` where
   raku rejects it.

## Known cosmetic divergence

When the single coerced value matches no `new` candidate, raku reports
`X::Coerce::Impossible` while mutsu surfaces the arity error from `new`
(`Too few positionals passed; expected 2 arguments but got 1`). Both reject the
call; only the message differs. Converting mutsu's error would mean catching a
failure from inside a user-written `new`, which risks masking genuine errors, so
it is left as is and pinned only as "dies".

Also still unsupported: the same call form on a **subset** (`subset Sm of Int
where * < 10; Sm(5)`), which raku accepts. That is a different mechanism (coerce
to the base type, then check the constraint) and is recorded in
`todo/tickets/dist-test-suite-failures-batch.md` alongside the other sweep
findings.

## Found by

The `--run-tests` axis of the real-dist compatibility sweep. `Locale::Dates`
graded `test_die`: `t/01-basic.rakutest` planned 8 and ran 0. Both of its files
now match raku exactly (24 subtests, 0 failures).

Pinned by `t/class-type-object-coercion-call.t` (11 subtests: `new`, `COERCE`,
`COERCE` winning over `new`, `CALL-ME` winning over both, the impossible-coercion
type and message, a qualified class name, single-List multi-argument semantics
both ways, and a bare type name still being the type object). All 11 identical
under raku.
