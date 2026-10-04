# Qualified Hash base calls, user multi fallback, and `|c` in methods

Found while working Intl::CLDR. `self.Hash::BIND-KEY(...)` and other qualified base calls on a
`Hash` subclass now run on the backing storage. A user `multi method AT-POS` on an `Array`
subclass falls back to the inherited method when no candidate matches. A method taking `|c` and
forwarding it to an `is rw` parameter now writes the caller's variable.
