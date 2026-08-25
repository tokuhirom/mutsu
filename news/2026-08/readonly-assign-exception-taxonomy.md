# "Cannot assign to this" is three different exceptions, not one

mutsu used to route every rejected assignment through `X::Assignment::RO`. Rakudo
does not: it splits them into three, and which one you get is a property of the
*lvalue*, not of the assignment site. Code writing
`CATCH { when X::AdHoc { ... } }` around a `for %h.values -> $v { $v = ... }`
typo therefore did not catch what it catches in real Raku.

## The taxonomy, established against `raku` v2026.06

Every shape below was probed against the reference implementation before any code
was written; the full survey is pinned by `t/readonly-assign-exception-taxonomy.t`,
which passes verbatim under both `raku` and `mutsu`.

**1. A readonly *binding* that still owns a container** — a non-`is rw` sub, block
or method parameter (positional or named), and a `for` loop's *named* alias over
anything at all (`-> $v` over `@a`, `%h.values`, `%h.keys`, a literal list):

> `X::AdHoc` — "Cannot assign to a readonly variable or a value"

`.VAR.^name` on these is `Scalar`: the container exists, it just refuses writes.

**2. A sigiled *variable* with no container at all**, bound straight to an
immutable value — `my $x := 42`, `my constant $PI = 3.14`, `our constant $Q = 1`,
a topic aliased to a literal (`given 5 { $_ = 6 }`, `for 1..2 { $_ = 5 }`,
`when Int { $_ = 6 }`), a whole-container topic (`given @a { $_ = 5 }`), and a
`Proxy` returned from a *sub call* (the return decontainerizes it, so the bind
sees the fetched value — the previously separate
`news/2026-08/bind-proxy-from-sub-return-value-closed.md` finding collapses into
this row):

> `X::AdHoc` — "Cannot assign to an immutable value"

**3. A name that denotes the immutable *value* itself** — a *sigilless*
`constant PI` term, a sigilless `my \c = 5` bind, a literal (`1 = 2`,
`'a' = 'b'`), an immutable container (`my %m := mix <a b>`, `my @a is List`,
`my constant @A`):

> `X::Assignment::RO` — "Cannot modify an immutable TYPE (VALUE)"

The sigil is what separates rows 2 and 3, and Rakudo really is that literal about
it: `my constant $PI = 3.14; $PI = 5` is `X::AdHoc`, while
`my constant PI = 3.14; PI = 5` is `X::Assignment::RO` with
"Cannot modify an immutable Rat (3.14)". A `$`-sigiled name is a *variable* and
goes through the `assign` op; a bareword is a *term* and the assignment reaches
`infix:<=>` on the value.

## The mechanism

mutsu's readonly bookkeeping was a `FxHashSet<Symbol>` of names — it recorded
*that* a name was readonly but not *why*, so the single shared check site had no
information to split on (and could not: the check receives a sigil-stripped name,
so `constant PI` and `constant $PI` arrive identical).

`ReadonlySet` is now a `FxHashMap<Symbol, ReadonlyKind>` with
`ReadonlyKind::{Alias, Immutable, ImmutableValue}` — exactly the three rows above.
The kind is recorded where the readonly-ness is *decided* (parameter binding,
`for`-alias marking, the `:=`-to-literal bind, the `constant` declaration, the
topic marking), which is the only place that knows it, and
`check_readonly_for_modify` simply dispatches on it. The scope journal
(`ReadonlyUndo`) carries the kind through mark/unmark/rollback so a restored
frame gets its original kind back, and the save/override/restore pairs around a
borrowed topic (`$_` in smartmatch, in a `.=` container topic, a `for` loop's
multi-param shadow) propagate the kind instead of collapsing it to a boolean.

For the `constant` declaration the source sigil is not in the name — mutsu's AST
strips `$` from scalar constants — so the compiler reads it from the
`__constant_sigil` trait the parser already records and emits the kind into
`OpCode::MarkVarReadonly`.

## `.VAR` fell out of the same property

`todo/tickets/bind-scalar-literal-var-name-not-int.md` was filed separately: `my
$b := 1; $b.VAR.^name` should be `Int` (no container) but mutsu answered
`Scalar`. That is the same fact from the other side, so `.VAR` now consults the
recorded kind: `Immutable`/`ImmutableValue` return the value itself, `Alias`
(and unmarked names) keep returning a `Scalar` container — which is what Rakudo
does for a readonly parameter and a `for @a -> $v` alias. `my $b := 1` is `Int`,
`my constant PI = 3.14` is `Rat`, `my constant $E = 2` is `Int`.

## `//=` / `||=` / `&&=` now really short-circuit

Splitting the classes surfaced a second, older bug. mutsu desugared
`$x //= v` to the flat `$x = ($x // v)`, which *stores unconditionally*; Rakudo
defines it as `$x // ($x = v)`, where the short circuit means no assignment
happens at all. That was invisible while every rejected assignment threw the same
exception, and wrong in several observable ways: `my $x := 42; $x //= 5` died
instead of succeeding, a `Proxy` LHS saw a spurious `STORE`, and a defined
readonly parameter could not be `//=`'d.

These three operators now desugar, for plain scalar targets, to a ternary whose
*store* branch is the assignment and whose *keep* branch is not. What the keep
branch yields is itself the container/no-container question from this ticket:
Rakudo returns the LHS container when there is one (so
`my $a = 52; ($a //= 42) += 10` leaves `62`) and the bare value when there is not
(so `my $a := 42; ($a //= 42) += 10` dies with `X::Assignment::RO` on the
returned value). Only the runtime knows which, so the keep branch asks —
`__mutsu_var_is_writable` consults the same readonly map. Both halves of
`roast/S03-metaops/misc.t` ("cover metaop call simplification optimization" and
"failure modes") pin that split, and `t/short-circuit-compound-assign.t` pins the
rest.

## Local tests that were asserting mutsu's wrong answer

`t/exception-types.t`, `t/typed-exceptions.t`, `t/readonly-method-params.t` and
`t/dotassign-store-and-container-topic.t` each asserted `X::Assignment::RO` for a
case Rakudo reports as `X::AdHoc`; all four were verified against `raku` and
updated. `t/exception-types.t` gained a positive `X::Assignment::RO` case (a
sigilless `constant` term) so the class is still pinned from the other side.
