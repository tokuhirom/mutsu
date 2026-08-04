# A built-in role brings its own roles with it

`class Fixed2 does Real { ... }` made `Fixed2 ~~ Real` true and
`Fixed2 ~~ Numeric` false. The transitive role walk in
`src/runtime/types/type_matching.rs` follows `registry().role_parents`, which
only records what *user* code declared, so `Real does Numeric` — a fact about a
built-in role — was nowhere in the picture. The same hole hid
`Mixy ~~ Baggy`, `Setty ~~ QuantHash` and `Baggy ~~ QuantHash`.

`Registry::builtin_role_parents` now names those four compositions, and
`Registry::role_parents_of` merges them with the declared ones so the walk sites
see one list. The type-object branch needed a little more: its registry walk is
gated on the constraint resolving to a *user-declared* role, and `Numeric` never
does, so a small dedicated pass over the built-in parents runs before that gate.

Found under the real `Test` module, and the symptom is the one this campaign has
now seen three times: `Test.rakumod` declares `multi sub is-approx(Numeric $got,
Numeric $expected, ...)`, so an `is-approx` over two `Fixed2` values matched
*none* of the module's candidates and fell through to mutsu's native provider,
which keeps a separate counter. `roast/S32-num/real-bridge.t` emitted all 201
assertions but numbered only 195 of them, and the module's `END` plan check
failed the file. It now passes under `MUTSU_REAL_TEST=1`.

Pin: `t/builtin-role-composes-its-own-roles.t` (all twelve assertions verified
against `raku`).
