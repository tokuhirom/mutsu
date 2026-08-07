# `$obj[*]` on an `is Array` / `is List` subclass instance gives Nil

A whatever-slice reads nothing from an instance whose elements live in the
backing `__mutsu_array_storage`, even though every other subscript form works:

```raku
class MA is Array { }
my $m = MA.new('a', 'b');
say $m[0].raku;      # "a"      -- correct
say $m[0,1].raku;    # ("a","b") -- correct
say $m[*].raku;      # raku: ("a", "b")    mutsu: Nil
```

The same holds for `class MyList is List { }`. A *plain* container is fine —
`my @a = 'a','b'; @a[*]` and `my $l = (1,2); $l[*]` both answer correctly — so
the gap is specific to the instance-subscript path, which resolves an integer or
list index against the storage but has no arm for `Whatever`. (The `:exists`
adverb path in `vm/vm_var_ops.rs` *does* have a `ValueView::Whatever` arm that
expands it via `.elems`; the plain read path needs the same.)

## Why it matters

It is the last failure of Cro's multi-value query/body family in
`t/http-request-parser.rakutest`:

```raku
is-deeply %hash<a>[*], ('1', '3', '4'), 'Indexing multi-value is correct (1)';
```

where `%hash<a>` is a `Cro::HTTP::MultiValue` (`is List does Stringy`). Its
construction and stringification work as of
`news/2026-08/is-list-subclass-takes-positional-new.md`; only `[*]` does not.

## Where to look

The instance subscript path — `try_compiled_method_or_interpret(... "AT-POS")`
and the `__mutsu_array_storage` delegation in `vm/vm_call_method_ops.rs` /
`vm/vm_call_method_compiled_interpret.rs`. A `Whatever` index should expand to
`0 ..^ .elems` before delegating, mirroring the `:exists` arm in
`vm/vm_var_ops.rs`.

Related: `todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md`.
