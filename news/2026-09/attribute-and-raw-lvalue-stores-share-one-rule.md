# Three lvalue stores that quietly disagreed now share one rule each

Three tickets filed on 2026-09-06 turned out to be three instances of the same
shape: a store that had grown its own private copy of a rule the neighbouring
store already implemented, and then drifted. Re-measuring each ticket's rows
against raku v2026.07 found the repros accurate but every diagnosis incomplete
— each bug was wider than its ticket said.

## 1. `f(...) = v` reported success and dropped the write

```raku
sub f(\x) is raw { x }
f(42) = 9;      # raku: dies "Cannot modify an immutable Int (42)"
                # mutsu: reported success, exit 0, nothing written
```

`OpCode::CaptureVarCell` is the container-mode tail of an rw-capable routine: it
resolves the tail's name to that variable's shared `ContainerRef` cell so
`sub f() is rw { $v }` hands the caller `$v`'s container. When the name had no
cell it *minted* one by boxing the frame's own local slot — correct for the
routine's own lexical (`sub a() is rw { my $y = 1; $y }`, which Rakudo lets you
assign to), but wrong for a parameter that bound an rvalue. A `\raw` parameter
aliases its argument: given a container it is that container, and given a value
it *is* that immutable value. Minting a cell for the second case produced a
container nobody else shared, so the assignment succeeded into nothing.

mutsu already knows which bindings are readonly — `CheckReadOnly` refuses
`x = 5` *inside* the same routine with exactly Rakudo's wording — but that
knowledge lives in two places: the `readonly_vars` registry (`ReadonlyKind`) and
the separate `__mutsu_sigilless_readonly::NAME` env marker a sigilless bind
uses. `check_readonly_for_incdec` consulted both; the capture consulted neither.
Both are now behind one predicate, `Interpreter::name_is_readonly_binding`, and
the capture asks it: a readonly-bound name yields its plain value, and
`assign_through_rw_result` refuses with `X::Assignment::RO`.

Re-measuring the neighbourhood found the ticket's two rows were four. A
*readonly* `$x` parameter was worse than the reduced case: `sub b($x) is rw { $x }`
handed out the caller's own container, so `b($v) = 9` silently wrote `9` into
the caller's `$v` where Rakudo refuses outright. A `Str` literal and a computed
argument (`f($q + 1) = 9`) were silently accepted too.

Making the refusal loud then exposed the opposite bug on the same path: an
`Array` or `Hash` argument *is* a container, and `f(@arr) = (7, 8)` must
list-assign into the caller's array (Rakudo does), but mutsu had been silently
dropping that write as well. `assign_through_rw_result` now replaces a real
`Array`/`Hash` result's contents in place, so the caller's container is
refilled. An immutable `List`/`ItemList` is deliberately excluded and still
refuses, as Rakudo does for `f((1, 2)) = 3`.

What now dies that used to "succeed": `f(42) = 9`, `f("s") = 9`,
`f($x + 1) = 9`, `ro-param($v) = 9`, and `++f(42)` — all with
`X::Assignment::RO`, "Cannot modify an immutable Int (42)". Rakudo words the
readonly-parameter case as "Cannot assign to a readonly variable or a value" and
the `++` case as an `X::Multi::NoMatch`; mutsu uses the immutable-value wording
for all of them. Every shape that legitimately wrote a container still writes
it, and binding the result (`my $r := f(42)`) is not an assignment and is
untouched.

## 2. The `is rw` method store and the accessor store disagreed twice

```raku
class A { }
class U { has A $.a is rw; method acc is rw { $!a } }
my $u = U.new;
$u.acc = A.new;
$u.acc = Nil;
say $u.a.^name;      # raku: A    mutsu: Nil
```

An `is rw` method whose body is a bare `$!attr` *is* that attribute's accessor,
so its store must behave exactly like the generated one. It did not: the method
store applied only the `is default(...)` half of the `Nil` reset and no type
check at all, so `$obj.acc = "str"` landed a `Str` in a `has Int $.n`. And the
accessor store, which the ticket called correct, had its own gap: it reset `Nil`
only when the attribute was *typed*, leaving an untyped `has $.x is rw` holding
a literal `Nil` where Rakudo restores `Any`.

Both halves are now one rule each — `check_attr_store_type` and
`attr_store_nil_default` — called from both stores. `Nil` restores
`is default(...)`, else the declared type object, else `Any`; the type check is
enforced wherever the value enters.

## 3. An `@`-sigil attribute was not list-assigned

```raku
class W { has @.w = 1..3 }   ; say W.new.w.raku   # raku: [1, 2, 3]  mutsu: [1..3,]
class F { has @.a }
say F.new(a => 5).a.raku;                          # raku: [5]        mutsu: 5
```

Two halves pulling in opposite directions. The parser ended a `has @.…` parse
with a blanket rewrite that wrapped any default expression not already
array-*shaped* into a one-element `Expr::ArrayLiteral` — at parse time it cannot
know whether the expression will produce a list, so a `Range`, `Seq`, `List`,
`Hash` or list-returning call became a single element. Meanwhile
`coerce_attr_value_by_sigil`, the runtime coercion that should have decided,
carried a hand-written partial copy of the list-assignment rule (`Array`,
`Range`, `Seq`) and fell through with `val.clone()` for everything else, so a
supplied scalar or type object was stored bare.

Raku assigns to an `@`-sigil attribute exactly the way `my @a = …` assigns, and
mutsu already has that rule in one place: `coerce_to_array`. The parse-time wrap
is gone and the `'@'` arm now delegates to it, with one explicit exception — a
genuinely deferred `Seq` still passes through unmaterialized, since this
coercion has no `&mut Interpreter` to force an iterator with and an infinite
source must not be reified here. That exception is what makes
`has @.l = (1..Inf).map(* * 2)` work now (it used to read back
`(..., Any, Any)`), and `has Int @.t = 1..3`, which used to die with a bogus
"expected Int but got Range", now list-assigns.

Sharing the rule exposed an ordering bug the old partial coercion had been
masking. `dispatch_new` ran the generic sigil coercion *before* handing an
`is Type` container attribute (`has @.a is Buf`) to its declared type. That was
harmless only because the old `'@'` arm let a `Buf` through untouched; with the
real list-assignment rule in place it became `[Buf]` — which is what
`my @a = $buf` gives in raku too — and `Buf.new(...)` was then rebuilt from the
wrapper. An `is Type` attribute is owned by its declared container type, so it
now sees the value as supplied and the generic coercion runs only when there is
no `is Type`. Caught by `t/ctor-istype-shaped-attrs.t`.

## Pins

`t/raw-sub-lvalue-refuses-value-result.t`,
`t/rw-method-attribute-store-shares-accessor-rule.t` and
`t/at-sigil-attribute-list-assign.t`, all byte-identical under mutsu and raku.
