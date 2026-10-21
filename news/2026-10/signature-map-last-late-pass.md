# A signature-bearing map block fires LAST with its final arguments on a late pass

When a prefix read had already consumed the last source element, the read that
finally saw the end of a `.map(-> $a, $b { LAST { ... } })` Seq ran no callback,
so its `LAST` found no parameters bound and ran with `Any` for both. The plan
slot of the Seq now keeps the arguments of the last call, and `LAST` binds
them, as Rakudo does.
