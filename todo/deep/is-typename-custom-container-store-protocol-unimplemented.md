# `my @var is CustomContainerClass = ...` never dispatches the class's `STORE`/overridden `Str` methods

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/subscripts.rakudoc:964`).

## Repro

```raku
role Logger { method log( Str $msg) {…}}

class ConsoLogger does Logger { method log ( Str $msg ) { "L $msg".say }}

class DNA {
    has $.chain;
    has Logger $!logger;

    submethod BUILD( :$chain, :$logger = ConsoLogger.new() ) {}

    method STORE (Str $chain where {
            /^^ <[ACGT]>+ $$ / and
            .chars %% 3
        },
        :$INITIALIZE --> DNA) {

        if ($INITIALIZE) {
            $!logger = ConsoLogger.new();
            $!logger.log( "Initialized" );
        }

        $!chain  := $chain;
        $!logger.log("Change value to $chain" );
        self
    }

    method Str(::?CLASS:D:) { return $!chain.comb.rotor(3).map( *.join("")).join("|") }
};

my @string is DNA = 'GAATCC';    # OUTPUT: «L Initialized␤L Change value to GAATCC␤»
say ~@string;                    # OUTPUT: «GAA|TCC␤»
@string = 'ACGTCG';              # OUTPUT: «L Change value to ACGTCG␤»
say  ~@string;                   # OUTPUT: «ACG|TCG␤»
```

- raku:
  ```
  L Initialized
  L Change value to GAATCC
  GAA|TCC
  L Change value to ACGTCG
  ACG|TCG
  ```
- mutsu (`target/debug/mutsu`):
  ```
  GAATCC
  ACGTCG
  ```
  (No logger output at all, and `~@string` just stringifies the plain assigned value instead of
  going through `DNA`'s custom `.Str` method.)

## Analysis

`my @string is DNA = 'GAATCC'` uses the `is` trait on a variable declaration to bind the variable's
*container* to a user-defined class (`DNA`) that implements the `STORE` protocol (a Proxy-like
custom container: assignment into the variable calls `DNA.STORE` with the RHS value, and reading
the variable's stringification dispatches through `DNA`'s `Str` method). mutsu appears to treat
`is DNA` as inert here — `@string` behaves like a plain container holding the raw assigned string,
with none of `STORE`/`Str`/the `INITIALIZE`-flagged first assignment being dispatched at all.

This is a non-trivial container/Metamodel feature: it requires the variable-declaration compiler
to recognize a custom `is <ClassName>` container binding (distinct from the ordinary `is
Array`/`is Hash`-style typed-storage traits, and distinct from role composition via `does`), route
every assignment through the class's `STORE` method (passing along any named arguments, like the
first-assignment `:INITIALIZE` seen implicitly here — worth checking how raku decides to pass
`:INITIALIZE` on the very first store), and route stringification/other coercions through the
class's own methods instead of the built-in container's.

## Why this is `todo/deep`

- Requires new variable-declaration-time dispatch (recognizing `is <UserClass>` as "bind this
  variable to a custom Proxy-like container implementing STORE", not just "use this native storage
  representation").
- Requires wiring assignment (`=`) to call through to `STORE` for such variables, which touches
  the core assignment-compilation path broadly (any `=` onto such a variable, from anywhere in the
  program).
- Interacts with the class/role/Metamodel system (the class implementing `STORE` may also
  `does` a `Logger`-style role, use `submethod BUILD`, `where`-constrained method signatures,
  `-->` return-type constraints, `::?CLASS:D:` invocant syntax — all need to already work
  correctly along the STORE-dispatch path).
- No existing ticket/ADR in the repo covers "custom `is ClassName` container binding on a
  variable declaration" (checked `todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md`
  and `todo/deep/p5tie-stash-bind-key-protocol.md` — related container-protocol themes, but neither
  covers this specific `is TypeName` variable-declaration binding).

## Affected files (starting point)

- `src/compiler/stmt.rs` — variable-declaration compilation, `is` trait handling
- `src/vm/vm_var_assign_ops.rs` (or equivalent) — assignment-to-variable execution, to add a
  STORE-dispatch branch for custom-container-bound variables
- `src/runtime/class.rs` — class/method resolution, to look up a `STORE` method on the bound class

## Suggested next step

Start by confirming the scope: is `is <UserClass>` (custom container binding via `STORE`) used
elsewhere in the doc corpus or in roast, or is this a rarely-exercised corner? A quick grep over
`raku-doc/doc/` and `roast/` for `method STORE` would help scope whether this is worth the
investment before committing to a design.
