# `first &test, LIST` follows the single-argument rule, so an itemized list stays one item

The listop form of `first` flattened every Array/List argument unconditionally, which is wrong in
two ways that rakudo's `first(Mu $test, +values)` does not share:

```raku
say (first { True }, $([1,2])).^name;          # rakudo: Array   mutsu before: Int
say (first { True }, [1,2].item).^name;        # rakudo: Array   mutsu before: Int
my @a = [1,2],;
say (first { True }, @a[0]).^name;             # rakudo: Array   (an element read is itemized)
say (first { True }, (1,2), (3,4)).^name;      # rakudo: List    mutsu before: Int
say (first { $_ ~~ Array }, 1, [2], 3).raku;   # rakudo: [2]     mutsu before: Nil
```

`+values` slurps under the single-argument rule: exactly one list argument is flattened into its
elements, and only when it is not itemized; two or more list arguments are each one element of
their own. `map` and `grep` already implemented both halves; `first` now does too (a `Slip`
still always flattens, and a bare Hash still flattens to its pairs while an itemized one stays
whole, the part of this that #10601's PR did for `Hash`).

Pinned by `t/collections/transform/first-listop-single-arg.t` (26 assertions, every one also checked
against `raku`, including that a single bare Array / List / Range / Seq still flattens).
Closes #10660.
