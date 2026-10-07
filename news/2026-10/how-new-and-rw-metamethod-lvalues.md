# `Metamodel::ClassHOW.new`, `$` as an rw tail, and `Type.^meta = v`

Three gaps found by the RedFactory distribution (ecosystem roulette):

- `Metamodel::ClassHOW.new` (and the other `Metamodel::*HOW` classes) now builds a
  HOW instance, and `.new_type` on that instance mints a type like the type object does.
- A bare `$` as the tail of an `is rw` method or sub is the implicit `state` cell, so
  `method m is rw { $ }; $o.m = 5` writes to it instead of dying with X::Assignment::RO.
- Assigning through a user-declared rw metamethod (`Type.^model = $model`) keeps the
  `^` in the lvalue name and passes the invocant like the rvalue call does.
