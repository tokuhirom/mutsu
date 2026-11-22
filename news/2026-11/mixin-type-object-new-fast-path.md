# `.new` on a role-mixed type object takes the default constructor's fast path

`(Base but R).new(...)` builds an instance of the composed class
`Base+{R}`. A per-type-object memo (#17331) already decided whether any user
`new` or `bless` could claim the call. Even so, every construction re-found the
composed class and ran the general constructor on it: about 3x the cost of
`.new` on a plain class.

The memo now also keeps the composed class. A hit constructs that class with
`try_native_default_construct`, the same call a `Class.new` site makes, and
falls back to the general constructor whenever the fast path declines.

Monad builds every value this way (`(pun but Monad::Maybe::Some).new(:$value)`):

- a construction on a mixed type object went from 40.8k to 28.1k
  instructions (callgrind);
- one Monad `t/14` chain iteration went from 8.25M to 7.85M.

`t/oo/mixin-type-object-new-fast.t` checks that repeated constructions stay
full constructions.
