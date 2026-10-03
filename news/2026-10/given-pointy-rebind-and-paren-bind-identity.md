# `with $!x -> $v { $!x := ... }` and `until (my $l := ...) =:= IterationEnd` now behave

Two bugs made Lines::Containing's iterator classes loop forever.

## A parenthesized `:=` declaration and container identity

`(my $line := $it.pull-one)` binds `$line` straight to the value, so
`$line =:= IterationEnd` must be True once the iterator is exhausted. This was
broken whenever the declaration had no local slot: inside a routine, a block,
or the condition of an `until` statement modifier. The store reached by name
(`SetGlobal`) never recorded that the name owns no Scalar container. Only the
slot store (`SetLocal`) recorded it. As a result, the identity check compared
a container against the value and every

```raku
++$n until (my $line := $iterator.pull-one) =:= IterationEnd || ...;
```

loop never terminated. The by-name store now records, or clears, the same
mark.

## A `given`/`with` pointy parameter no longer undoes a rebind of its source

At block exit, `given $!line -> $l { ... }` writes the parameter's final value
back to the topic's source. When the block had rebound the source itself
(`$!line := Str`), that write-back put the old value back, because `$l` still
held it. The write-back is now skipped when a scalar pointy parameter still
holds its entry value: such a parameter was never written, so the source keeps
whatever it holds now.

As a result, Lines::Containing's `t/01-basic.rakutest` passes 34/34 under
mutsu. Before, it hung on the first `:count-only` call.
