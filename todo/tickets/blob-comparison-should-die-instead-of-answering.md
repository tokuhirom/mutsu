# Blob comparisons that rakudo rejects with a type error, mutsu silently answers

Two families of Blob comparison still diverge from rakudo after the
`infix:<eq>`/`infix:<ne>` cross-Blob-type byte-comparison fix
(`news/2026-08/blob-eq-compares-bytes-across-blob-types.md`). Both of them are
cases where **rakudo throws and mutsu returns a value**, so they are strictly
riskier to change than the `eq`/`ne` fix was: a new throw can abort a whole test
file mid-run, and the wrong answers are currently "harmless" in the sense that
nothing in the whitelisted roast suite depends on them.

## 1. A non-`utf8` Blob in a string context must throw `X::Buf::AsStr`

`Buf`/`Blob` (every parameterisation except `utf8`) has no usable `.Str`;
rakudo dies rather than stringifying it.

```
$ raku -e 'say Buf[uint8].new(104,105) eq "hi"'
Stringification of a Buf[uint8] is not done with 'Stringy'. The
'decode' method should be used to convert a Buf[uint8] to a Str.

$ ./target/debug/mutsu -e 'say Buf[uint8].new(104,105) eq "hi"'
False
```

mutsu falls through to `to_str_context()`, which for a Buf renders the *gist*
(`Buf[uint8]:0x<68 69>`, `src/value/display.rs:807-828`), so the comparison
answers `False` for every Str. `.Str`/`.Stringy` on a Buf *already* throws
`X::Buf::AsStr` in mutsu (`src/builtins/methods_0arg/mod.rs:1331-1355`) — the
comparators simply never call it, because `coerce_stringy_operand` only
dispatches a *user-defined* `Stringy`/`Str`, not a native one.

The fix is to make the string comparators (and `leg`, `coll`, `unicmp`, the
`[eq]`/`[lt]` reduction table, and the `to_str_context()`-based sort
comparators) raise `X::Buf::AsStr` when a non-`utf8` Blob reaches a genuine
string context. That is a wide, throw-introducing change touching every site
listed under "the other decision points" below, which is why it is not folded
into the `eq`/`ne` fix.

## 2. Ordering ops across two *different* Blob types must throw

Rakudo's `Blob` ordering candidates (`infix:<lt>`/`gt`/`le`/`ge`/`cmp`, from
`SETTING::src/core.c/Buf.rakumod:1784`) are effectively same-type only: the
declared signature is `(Blob:D $a, Blob:D $b)` but the body binds the second
operand to a same-type parameter, so a mixed pair fails the type check.

```
$ raku -e 'my $u = "hi".encode; say $u lt Buf[uint8].new(104,105)'
Type check failed in binding to parameter 'other'; expected utf8 but got Buf[uint8]

$ raku -e 'my $u = "hi".encode; say $u cmp Buf[uint8].new(104,105)'
Type check failed in binding to parameter 'other'; expected utf8 but got Buf[uint8]

$ ./target/debug/mutsu -e 'my $u = "hi".encode; say $u lt Buf[uint8].new(104,105)'
False
$ ./target/debug/mutsu -e 'my $u = "hi".encode; say $u cmp Buf[uint8].new(104,105)'
Same
```

Same-type ordering (`Buf[uint8] lt Buf[uint8]`, `utf8 lt utf8`) is byte-wise in
both implementations and already agrees, so only the *mixed* pair diverges.
`eq`/`ne` deliberately do **not** belong here: they have a genuine
`(Blob:D, Blob:D)` candidate and compare bytes across types (measured; that is
the behaviour the accompanying fix implements).

Note that `leg` is a third case again: rakudo routes it through `Str`, so
`utf8 leg utf8` works (both decode) but `Buf[uint8] leg Buf[uint8]` throws
`X::Buf::AsStr`. mutsu's `exec_leg_op` has no Blob handling at all and compares
the two gists.

## The other decision points that would need to move together

Mapped while fixing `eq`/`ne`; every one of these compares two Blobs by their
hex-gist string today, so they are all wrong in the same way and should be
settled by whatever shape this ticket lands:

- `src/vm/vm_comparison_order_ops.rs` — `exec_leg_op`, `spaceship_ordering`
  (reached by `before`/`after`), `exec_coll_op`, `exec_unicmp_op`.
- `src/vm/vm_comparison_ops.rs` — `cmp_values` (list/range operands).
- `src/runtime/ops_reduction.rs:548-574` — the `[eq]`/`[lt]`/`[leg]`/`[cmp]`/
  `before`/`after` reduction and metaop table.
- `src/runtime/test_functions/comparison.rs:77-79`, `:176-178` — `cmp-ok`'s
  `"eq"`/`"lt"` handlers.
- `src/runtime/methods_collection_ops/sort.rs:350, 368, 429, 445` — the
  fast-path `.sort({ $^a leg $^b })` inline comparators.

Two predicates also need reconciling first: `Interpreter::is_buf_value`
(`src/vm/vm_coerce_concat_ops.rs:421`) misses `utf32` and the `bufN` spellings
that the broader `crate::runtime::utils::is_buf_or_blob_class`
(`src/runtime/utils.rs:237`) covers, and `src/vm/vm_smart_match.rs:335-365`
carries a third, hand-inlined copy of the same eight-arm list. Any change here
should collapse them to one predicate rather than adding a fourth copy.

## Minimal repro

```raku
my $u = "hi".encode;
my $b = Buf[uint8].new(104, 105);
say $b eq "hi";   # raku: X::Buf::AsStr   mutsu: False
say $u lt $b;     # raku: type check failure   mutsu: False
say $u cmp $b;    # raku: type check failure   mutsu: Same
say $b leg $b;    # raku: X::Buf::AsStr   mutsu: Same
```
