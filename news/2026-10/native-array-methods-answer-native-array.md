# A native array's sort/squish/reverse/unique/head/tail/rotate answer a native array

On `my int @a`, rakudo's `sort`, `squish`, `reverse`, `unique`, `head`, `tail` and `rotate` rebuild
an `array[int]` (and `values` is a plain `List`), where mutsu answered a `Seq`: Code::Coverage's
`%!covered{$key} := $lines.sort.squish` followed by `$lines.push` died with
"No such method 'push' for invocant of type 'Seq'". `method_table::native_array_result::retype`
now retypes those answers to the receiver's native array type (boxed `Int @` and untyped arrays
still answer a `Seq`). It runs where a method-table row answers (which the method-site lane uses)
and at the VM's `CallMethod`/`CallMethodMut` sites and the compiled-method entries, because the
full path reaches the same elements through element producers that never pass a row.
Covered by `t/collections/array/array-sort-keeps-element-type.t`.
