# A `Pair` held in a variable is passed as a *named* argument

> **Design decided (2026-08-08):** the full investigation and the phased design
> (P1 method-boundary parity → P2 slip/capture rules → P3 minting inversion →
> P4/P5 cleanup) are recorded in
> [ADR-0021](../../docs/adr/0021-argument-namedness-is-a-call-site-property.md).
> Key findings that supersede parts of the text below: the *function* path is
> already correct (every non-syntactically-named arg gets `ContainerizePair`,
> `src/compiler/helpers_call_args.rs:256-258`) — the method path just lacks the
> same one normalization; `|@l` slips and Capture forwarding misclassify too;
> and runtime-invoked calls (`$bag.pairs.map(&show)` dies today) prove the
> minting default must flip as well. The Set/Bag/Mix abort has a root cause
> (four `ValueView::Pair`-only consumer arms) listed in the ADR.

In Raku, whether an argument is named is a property of the **call site**, not of
the value. Only a literal `key => value` / `:key(...)` / `:$x` written directly
in the argument list is a named argument; a `Pair` that arrives through a
variable, an array element, `%h.pairs`, or `Pair.new` is an ordinary positional
argument (you need `|$p` to make it named).

mutsu encodes named-ness in the **value** instead: `ValueRepr::Pair(String, _)`
means "named argument" and `ValueRepr::ValuePair(Value, Value)` means
"positional argument" (see the comment in
`src/runtime/types/args_matching.rs:99`, and `Value::is_string_pair_value`,
which 32 call sites consult to split an argument list into positionals and
nameds). The parser marks the few syntactic forms it knows are positional
(`Expr::PositionalPair`, for a non-bareword key or a space-parenthesized
`f (a => 1)`) and everything else defaults to the named flavour.

So *any* `Pair` with a `Str` key that reaches a call through a variable is
misread as a named argument:

```raku
class C {
    multi method m(Pair $p) { say "Pair" }
    multi method m(Str $s)  { say "Str"  }
}
my $c = C.new;

$c.m(Pair.new('a', 1));           # Cannot resolve caller m(C:D: :a(Int))
my $p = a => 1;      $c.m($p);    # Cannot resolve caller m(C:D: :a(Int))
my $q = :a(1);       $c.m($q);    # Cannot resolve caller m(C:D: :a(Int))
my @l = [a => 1];    $c.m(@l[0]); # Cannot resolve caller m(C:D: :a(Int))

my $r = (a => 1);    $c.m($r);    # works — parenthesised, so PositionalPair
```

`raku` prints `Pair` for every one of these. `tmp/hdr15.p6` is the matrix above.

## What is already fixed

The **hash-derived** half is done: entries read out of a Hash (`%h.pairs`,
`%h.List`, `%h.antipairs`, `%h.invert`, iterating `%h`) are now built as
positional pairs, so `$c.m(%h.pairs[0])` binds the `Pair` candidate — see
`news/2026-08/hash-derived-pairs-are-positional-arguments.md` and
`t/hash-pair-is-positional-argument.t`. What remains is every *other* way a Pair
is minted: `Pair.new`, a fat-arrow or colonpair assigned to a variable, and a
fat-arrow inside an array/list literal (`MakePair`).

One deliberate exclusion from that slice: `quanthash_typed_pair` (Set/Bag/Mix
entries) was left on the named flavour. Switching it made the vendored Cro
suite's `http-middleware.rakutest` abort mid-file, so it needs its own
investigation rather than being swept along.

## Why it matters

This is not an exotic corner. Any library that iterates pairs and hands them to
a typed multi hits it. The case that surfaced it is `Cro::HTTP::Client`:

```raku
method !set-headers($request, @headers) {
    for @headers {
        when Pair | Cro::HTTP::Header { $request.append-header($_) }
        default { die X::Cro::HTTP::Client::IncorrectHeaderType.new(what => $_) }
    }
}
```

`when Pair` matches, then `append-header($_)` dispatches with `$_` read as a
named argument and no candidate matches. `headers => %h` reaches `set-headers`
through `%h.List` and so works now; `headers => [Authorization => '...']`
reaches it through an array literal, whose elements `MakePair` still mints as
named, and still dies. Minimal repro: `tmp/hdr2.p6` (the `list of pairs` line;
mutsu: `X::Multi::NoMatch` on `append-header`, raku: 200).

## Why this is a deep ticket

The fix is to move named-ness from the value to the call site, and both
directions are wide:

1. **Mark named args at the call site.** `Expr::Call` carries `args: Vec<Expr>`
   with no named/positional distinction at all — the distinction only exists in
   `ast::CallArg`, which this node does not use. Either teach the call nodes to
   carry a named mask (and thread it through every `CallFunc`/`CallMethod`
   opcode, minding the 48-byte `OpCode` budget), or emit a normalization opcode
   after each non-syntactically-named argument. The latter costs an extra
   instruction on the hottest path in the VM (`f($x)`), so it needs a static
   "can this expression even yield a Pair" filter to be affordable.

2. **Invert the value default.** Make `MakePair` (from `TokenKind::FatArrow`,
   `src/compiler/expr_helpers.rs:199`) emit the *positional* flavour everywhere
   except directly in an argument list, and make every Pair-producing builtin
   (`Pair.new`, `.pairs`, `.kv`, `.antipair`, hash iteration, ...) produce the
   positional flavour too. Zero runtime cost, but it changes what ~all Pair
   construction sites produce and interacts with all 32 `is_string_pair_value`
   consumers.

Either way the blast radius is the whole dispatch/binding layer, so it wants an
ADR and a full roast run rather than a local patch. Note that direction 2 is
closer to the Raku model only by accident: the real model is that the *value*
has no named-ness at all, so direction 1 is the honest one and direction 2 is a
cheaper approximation of it.

## Related

`%h.List` returning a one-element list holding the Hash (instead of the Hash's
pairs) was a second, independent reason `headers => %h` failed; that one is
fixed — see `news/2026-08/hash-list-coercion-yields-pairs.md`.
