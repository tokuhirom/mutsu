# `$s.substr-rw(...) = v` evaluates to the assigned value

An assignment through a `substr-rw` lvalue now evaluates to the substring it
stored, like the `substr-rw` Proxy's FETCH in Rakudo:
`say ($s.substr-rw(0, 1) = "Y")` prints `Y`, not the whole rewritten `Ybc`
(#10583). The write-back is unchanged. `assign_substr_rw`, the one routine
behind both the method and the sub form, was returning the rewritten invocant
as the assignment's value.

While working on this we found that the write-back never reaches an `is rw`
attribute accessor or an `is rw` parameter invocant. That is filed as #10790.
