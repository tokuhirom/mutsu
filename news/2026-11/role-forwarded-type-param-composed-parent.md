# A role's forwarded type parameter reaches the composed parent role

`role Q[::U] does R[U]` composed as `class B does Q[Array]` left R's `T` bound to a stale `Str`
placeholder, so `T.new` in R's `BUILD` and methods answered an empty string. Composing the parent
role into the class now binds the parent's own parameter to the argument Q was composed with
(`T` = `Array`), as a literal `does R[Array]` always did (#15588).
