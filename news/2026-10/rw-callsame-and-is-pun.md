# `callsame` in an rw AT-KEY/AT-POS stays assignable; `^is_pun` / `^pun_source`

`method AT-KEY($k) is rw { callsame }` on an `is Hash` subclass (and `AT-POS` on `is Array`)
now returns the element's container from the base candidate, so `$obj<k> = v` assigns. Added
`Metamodel::ClassHOW.is_pun` and `pun_source`. Both were needed by PDF::COS::Tie (PDF dist).
