# Whatever-curry no longer blocked by `xx *` in an argument

`*.push(1 xx *)` died with "No such method 'push' for invocant of type 'Whatever'" because the
`xx`-opt-out check (`contains_xx_with_bare_whatever`) recursed into method-call, call and subscript
arguments. It now follows only the curry spine (operands and invocation/subscript targets), the same
positions `contains_whatever` follows, so `*.push(1 xx *)` is a `WhateverCode` as in Rakudo.
