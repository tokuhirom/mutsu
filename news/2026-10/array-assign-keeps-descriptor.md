# Whole-container assignment keeps the destination's descriptor

`my @e; my @a = 1; @e = @a; @e.name` answered `@a`: the in-place reassign cloned
the source's `ArrayData` over the destination, descriptor name included. The
descriptor belongs to the container being assigned into, so `@e.name` is now `@e`,
as in Rakudo. The same holds for `%h`-style hashes and for assignment over a
non-empty container (#12638).
