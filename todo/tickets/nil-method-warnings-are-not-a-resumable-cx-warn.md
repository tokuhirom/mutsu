# `Nil.Real`/`.Int`/`.Str`'s warning is not a catchable `CX::Warn` control exception

Found while retiring the native `Test::Util` overrides
(`todo/tickets/retire-native-test-util-overrides.md`). When
`t/any-type-object-int-coercion.t` and `t/bound-nil-method-warn.t` are
switched to import the real `Test::Util` (`use lib
$*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib"); use
Test::Util;`), their `Nil.Real` / `Nil.Int` / `Nil.Str` (and bound-`Nil`
equivalents) warning assertions fail — but the *same* files pass 100% under
real `raku` with the same import. mutsu's own `warns-like` (used when the
file does NOT import `Test::Util`, so mutsu's `is_test_function_name`
fallback provides it) continues to report these as passing, so the
divergence is specifically in how the underlying warning is *raised*, not
in whether a warning fires at all.

## Repro

```raku
use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;
plan 1;
warns-like { Nil.Real }, *.contains('Nil' & 'numeric'), 'Nil.Real warns';
```

mutsu: `not ok 1` — both `code threw a warning` AND `warning message passes
test` fail (the subtest never sees the warning at all).
raku: `ok 1`.

## Root cause hypothesis

Real `Test::Util`'s `warns-like` (`roast/packages/Test-Helpers/lib/Test/Util.rakumod:315-325`)
catches the warning via:

```raku
CONTROL { when CX::Warn { $did-warn = True; $message = .message; .resume } }
```

i.e. it relies on `warn()` raising a **resumable `CX::Warn` control
exception** that a `CONTROL` block can intercept. mutsu's native `Nil.Real`
/ `.Int` / `.Str` coercions apparently print/raise their "Use of Nil in
numeric/string context" warning through a different internal mechanism that
does not surface as a `CX::Warn` a `CONTROL` handler can see — the message
appears on stderr (confirmed: `t/any-type-object-int-coercion.t` without
`Test::Util` prints `Use of Nil in numeric context` directly, so mutsu DOES
warn, just not through the catchable-control-exception path).

Narrower than it first looks: `Mu.Numeric`'s uninitialized-value warning
(`t/type-object-numeric-coercion.t`) already works correctly through real
`Test::Util`'s `warns-like` — only the `Nil`-specific coercion warnings
(`Nil.Real`/`.Int`/`.Str`, plain and bound-via-`:=`) are affected. So the
gap is specifically wherever the interpreter special-cases `Nil` numeric/
string coercion warnings, not the general Cool-autoboxing warning path.

## Where to look

Find the `Nil` coercion warning call sites (search for "Use of Nil in" in
`src/`) and compare how they raise the warning against whatever raises
`Mu.Numeric`'s uninitialized-value warning (which IS caught correctly by
`CONTROL { when CX::Warn }`). The `Nil` sites likely bypass the shared
warn-raising helper the `Mu`/`Any` path uses.

## Scope

Not fixed as part of `retire-native-test-util-overrides` — that ticket
reverted the `use Test::Util` addition for these two files (keeping them on
mutsu's native `warns-like` fallback, which already passes) rather than
block the mechanical migration on this deeper interpreter gap. The other 7
files migrated in that PR (`is_run`, `is-eqv`, `is-path` callers) are
unaffected.
