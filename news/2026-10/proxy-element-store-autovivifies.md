# An element store through a Proxy holding no aggregate autovivifies it

`$obj.accessor[0] //= 7` and `$obj.accessor<k> = 1`, where the rw accessor
returns a Proxy whose FETCH answers a type object (an unset array or hash
attribute behind PDF's `is entry` accessors), stored into a throwaway container
and left the attribute `Any`. The store now autovivifies the aggregate and
STOREs it through the Proxy, then writes into what the Proxy holds (#14083:
`PDF::COS::Type::XRef.encode` died with `/XRef mandatory /Index[0] entry is
missing`).
