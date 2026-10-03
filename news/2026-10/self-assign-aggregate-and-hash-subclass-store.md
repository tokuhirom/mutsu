# `self = ...` on a mixed-in aggregate, and `STORE` on an `is Hash` instance

Hash::LRU applies its roles to a plain Hash and empties it with
`method clear() { self = () }`. mutsu accepted `self = ...` only when the
invocant arrived through a container cell (a caller's `@items`). The
role-mixed Hash arrives as the value itself, so `clear` died with "Cannot
modify an immutable value". An Array or Hash value shares its backing node
with every holder, so it is now reassigned in place either way. A container
object (an `is Hash` instance) is assigned through its `STORE`, as `=` on any
container is.

That exposed a second bug. `STORE` on an `is Hash` instance rebuilt the
instance with a fresh backing hash and wrote it back only under the name the
call was made on. A `self.STORE(...)` inside a method was therefore lost to
the caller. `STORE` now replaces the existing backing hash's contents in place.

Hash::LRU's `t/Cache-LRU.rakutest` now passes 38/38 (it died at test 31).
