# `.^can` on a class hands back the same Method objects, memoized

`.^can` built a fresh Method object (attribute map, Signature, callable) for
every candidate on every call: about 100,000 instructions for
`$type.HOW.^can("as")`. rakudo hands back the class's own Method objects, the
same ones each time. A class or instance receiver's answer depends only on
(class, method) and the registry, so it is now memoized under the method
generation every `.^add_method`, `.wrap`, `augment` and role composition
bumps — the same scheme `.^lookup` / `.^find_method` already use. Mixins, enum
values, `Attribute` objects (whose `DEPRECATED` is per-object) and a call made
while a multi of that name is still being declared keep the uncached path.

Red's `Red::Column.gist`, which every column's `WHICH` goes through, asks
`$model.HOW.^can("as")` each time.
