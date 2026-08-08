# `$obj[*]` answers on an `is Array` / `is List` subclass instance

A whatever-slice read nothing from an instance whose elements live in the backing
`__mutsu_array_storage`, even though every other subscript form worked:

```raku
class MA is Array { }
my $m = MA.new('a', 'b');
say $m[0].raku;      # "a"        -- correct
say $m[0,1].raku;    # ("a","b")  -- correct
say $m[*].raku;      # raku: ("a", "b")    mutsu: Nil
```

The instance-subscript path in `vm/vm_var_index_ops.rs` resolved an integer, a
list of indices, a fractional index and a `Str` key against the storage, and it
had a `Whatever` arm for a *tied Associative* instance (`%h is Foo; %h{*}`,
guarded on a user `keys` method) — but nothing for a positional one, so `*` fell
through to the Nil default. The `:exists` adverb path
(`instance_exists_pos_result`) had done the expansion all along, which is why
`$m[*]:exists` answered while the plain read did not.

## Fix

A `(Instance, Whatever)` arm for a positional subscript on an instance carrying
`__mutsu_array_storage`: expand `*` against `.elems` and read each index through
the storage delegation, returning a `List` as raku does.

It is deliberately **not** widened to every `does Positional` instance. raku
answers `Own.new(...)[*]` with a one-element list holding the object itself when
the class supplies only `AT-POS`/`elems`, so a blanket arm would have traded one
divergence for another. (mutsu still says Nil in that case; it is a separate,
much narrower gap.)

## Effect

The last failure of Cro's multi-value query/body family in
`t/http-request-parser.rakutest`:

```raku
is-deeply %hash<a>[*], ('1', '3', '4'), 'Indexing multi-value is correct (1)';
```

where `%hash<a>` is a `Cro::HTTP::MultiValue` (`is List does Stringy`). Together
with `news/2026-08/rw-accessor-writeback-preserves-a-shared-cell.md` the file
goes from 334 pass / 7 fail to **342 pass / 2 fail**.

Pinned by `t/whatever-slice-on-positional-instance.t`.

Related: `todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md`.
