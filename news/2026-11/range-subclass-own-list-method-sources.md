# A Range subclass's own `list` answers `join`/`Array`/`List`/`pick`/`Str`, not `map`/`grep`/`sort`

A `Range` or `Seq` subclass keeps the base type's own `iterator`, so declaring only `list` must not
redirect the methods rakudo writes on `self.iterator`. mutsu sent every `Any` list method to the
subclass's `list` through the plain-class rule (`map`, `grep`, `sort`, `first` answered `(7 8)`-shaped
results) while `join`, `Array`, `List`, `pick` and `Str` ignored it. The instance dispatch now splits them
as measured against rakudo: the `self.iterator` methods read the carried range, the five `self.list`
methods read the subclass's `list`, and `Seq` stays on the range (#16022). `(0..3).squish` returning the
range itself is a separate bug, filed as #16038.
