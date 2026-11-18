# The implicit `Any` check of an untyped parameter is answered once per class

Every untyped routine parameter is typed `Any`. Binding an instance of a user class to one ran the
full `type_matches_value("Any", ...)` on each call:

- the instance memo declined the key, because `Any` is not a registered class;
- the check then walked the receiver class's MRO by name.

That is about 7.8k instructions per argument, and a method called as a value pays it for its
invocant too.

`type_matches_value_why` now asks `instance_matches_core_any` first. That method answers whether a
user class's MRO reaches the core `Any`, once per class for each registry write generation, under a
new `MemoCheck::CoreAny` key. A class whose MRO reaches `Any` matches it whatever else the name may
be bound to, because an import alias of `Any` is consulted only after the core meaning failed.
So a cached `true` is the whole answer. A class declared `is Mu` still takes the full check and is
refused as before. A program that declares its own `Any` class skips the memo.

Callgrind slope (profiling build, N 300 -> 900) of `f($o); $a.m($o)` with untyped parameters and
`$o` a user-class instance: 31.4k -> 18.0k Ir per iteration. A Method::Protected-wrapped call
(`tmp/mp9.raku`): 156.9k -> 150.9k Ir. `t/routines/signature/untyped-param-implicit-any.t` pins both
answers. Refs #12875.
