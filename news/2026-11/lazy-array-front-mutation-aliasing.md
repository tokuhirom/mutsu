# A lazy Array is shared: shift/unshift and `@` parameters alias it

A generator-backed lazy array (`my @y = [1 .. *]`, `[1, 2 ... *]`) used to lose front mutations in three places:
`$d.shift` on a scalar-held lazy array died with "No such method 'shift'", `$pair.value.unshift(0)` had no variable to
rebuild, and a named `:@a` parameter bound a detached `List` view of the array, so the callee's `@a.shift` never reached
the caller (`&f.assuming(:@ids)` over `[1 .. *]` answered `1,1,1`). `shift`/`unshift`/`prepend` on such a list now
rewrite its cache (and a sequence spec's generator history) in place, so every holder sees the new front, and an `@`
parameter binds a lazy Array as itself. JSON::RPC `t/00-client.rakutest` is 39/39 like rakudo (#16895).
