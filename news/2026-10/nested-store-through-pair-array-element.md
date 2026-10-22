# A chained store steps through a Pair held by an array element

`@a[1]<s><e> = "v"` with `@a[1] = s => {...}` died with "Cannot modify an
immutable Pair": the chain walk checked the array element for a descendable
container without telling it which key the next subscript names, so a Pair was
refused and then replaced by a fresh container instead of being stepped
through. PDF's `%ast<ind-obj>[2]<stream><encoded>.substr-rw(3, 1) = 'x'`
(`t/cos-type-objstm.t`) is that shape (#14083). The walk now passes the next
key and leaves a Pair that holds the addressed container in place.
