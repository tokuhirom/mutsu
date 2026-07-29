# DBIish runs end-to-end on PostgreSQL — seven general fixes

A day after the MariaDB milestone, the same DBIish 0.6.8 prepared-statement
lifecycle now runs against a live PostgreSQL 16 (`DBIish.connect('Pg', …)` →
DDL → `PQprepare` → typed-parameter `PQexecPrepared` → `allrows`/`row` with
type conversion → `dispose`), byte-identical to Rakudo. An extended check —
bool/bigint/float8/numeric/text/NULL round-trips, column metadata, `rows`,
SQL-level transactions, a caught `X::DBDish::DBError::Pg`, `server-version` —
is also byte-identical. As with MariaDB, every fix is general-purpose; nothing
special-cases DBIish.

The Pg driver exercises a different surface than mysql, which is why seven new
bugs surfaced:

1. **A native method's invocant is its first C argument even when the
   signature leaves it implicit.** `DBDish::Pg::Native` declares
   `method PQstatus(--> int32)` on a `repr('CPointer')` class — no
   `::?CLASS:D:` spelled out, unlike the mysql driver. mutsu only synthesized
   the leading pointer parameter for an explicit invocant, so every libpq call
   failed with an arity error. The registration now inserts the pointer slot
   whenever the routine is a method. (`t/nativecall-method-implicit-invocant.t`)

2. **The native `str` type maps to `char*`.** `sub PQconnectdb(str --> PGconn)`
   silently skipped native registration because lowercase `str` was not in the
   C-type table.

3. **An undefined element of a `CArray[Str]` is a NULL `char*`.**
   `PQconnectdbParams` terminates its key/value arrays with `$keys[$i] = Str`;
   mutsu stringified the type object into a literal `"(Str)"`, which libpq
   rejected as an invalid connection option.

4. **An undefined `CArray[T]` argument is a genuine NULL pointer.** DBDish::Pg
   passes `Null` (a type object) for `paramLengths`/`paramFormats`; mutsu
   handed over a pointer to an empty buffer instead, libpq took the non-NULL
   `paramFormats` as a real per-parameter array, read garbage, and segfaulted.
   (Both CArray cases: Rust unit tests in `runtime/nativecall.rs`.)

5. **Action methods fire for subrules inside positional capture groups, and
   their `.made` is visible through `$0[…]`.** The Pg placeholder tokenizer is
   `token TOP { ^ ( <normal> | <placeholder> )* $ }` with per-token actions
   joined in the TOP action — mutsu never dispatched actions into `( )` groups
   and never stored the updated children back, so `pg-replace-placeholder`
   returned an empty string and every `prepare` died. The reduce-time isolation
   copy of the actions object also needed a fresh instance id — sharing the id
   let the post-dispatch env refresh silently swap the copy back to the real
   object, tripling attribute mutations (`t/grammar-actions-positional-group.t`,
   and `t/grammar-reduce-time-dynvar.t` test 5 pinned the leak).

6. **Private method calls resolve lexically.** `StatementHandle`'s BUILD calls
   `self!get-meta` inside a block passed to `$!parent.protect-connection`; the
   runtime caller-class check saw the parent's class and refused the call. A
   closure whose captured `self` is an instance of the owning class is code
   written inside that class, and is allowed. (`t/private-method-call-in-closure.t`)

7. **A method carrying a LEAVE phaser keeps the value of a tail `if`.**
   `prepare` ends in `LEAVE { $result.PQclear … }; if $result && $result.is-ok
   { StatementHandle.new(…) } else { … }` and returned Nil — the
   phaser-carrying compile path lacked the value-position statement forms
   (`if`/block/decl) the plain path reifies. (`t/method-leave-tail-if-value.t`)

Three more fell while checking the extended surface:

- **A native handle is tagged with the name its class is registered under.**
  `PQprepare(--> PGresult)` returned an instance tagged with the short name
  while the class registered package-qualified (`DBDish::Pg::Native::PGresult`),
  so the ordinary Raku method `is-ok` on the handle failed to dispatch.
  (`t/nativecall-cpointer-class-in-module.t`)

- **Per-class BUILDALL: a custom BUILD takes over named-arg auto-assignment
  only for its own class's attributes.** `X::DBDish::DBError::Pg` computes
  `$!sqlstate is required` in its BUILD while its parent's required attributes
  arrive as named args; mutsu suppressed auto-assignment MRO-wide whenever any
  BUILD existed, mis-raising `X::Attribute::Required`.
  (`t/new-build-parent-required-attr.t`)

- **A sigilless binding in an `if` condition binds the value itself.**
  `DBDish::StatementHandle.row` is `if my \r = self._row { … r.Array }`; mutsu
  itemized `r` into a scalar container so `my @row = $sth.row` got one nested
  element. Both the `my \r = …` form and the pointy `if EXPR -> \r { }` form
  are fixed. (`t/if-pointy-sigilless-binding.t`)

One upstream bug found while testing: DBIish 0.6.8's `commit`/`rollback`
*methods* die under Rakudo too (`$!parent` is the driver, which has no
`protect-connection`), so SQL-level `BEGIN`/`COMMIT`/`ROLLBACK` is the parity
surface.

Reproduction: docker `postgres:16` on port 15432, `tmp/dbiish-e2e-pg.raku` and
`tmp/dbiish-pg-extra.raku` under `tmp/dbslot/DBIish-0.6.8` with the usual `-I`
lines. With this, DBIish is end-to-end on both of its two most important real
drivers; what remains for the battery is bundling
(`todo/tickets/dbiish-blockers.md`).
