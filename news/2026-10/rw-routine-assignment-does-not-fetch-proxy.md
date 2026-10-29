# Assigning through an `is rw` routine no longer FETCHes the Proxy it stored into

`f(...) = v` on an `is rw` multi reached through a `proto` ran the Proxy's
`STORE` and then its `FETCH`, because the interpreter terminal for
`__mutsu_assign_named_sub_lvalue` auto-fetched a Proxy result like any other
rw call. Rakudo runs the STORE only: the value of the assignment is the
container, and a sunk statement never reads it. The assignment builtins are
now exempt from that fetch, as `substr-rw` and `callwith` already were.

P5substr's `substr($s, 7) = 'gap'` (an out-of-range store whose FETCH threw a
second `X::OutOfRange`) reaches its `.resume` correctly with this and #15555:
`t/01-basic.t` 19/19, like raku.
