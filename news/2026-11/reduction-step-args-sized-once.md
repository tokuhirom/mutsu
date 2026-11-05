# A reduction step no longer reallocates its argument list

Every step of a left-associative `[op]` fold built its argument `Vec` with
`vec![acc]` (capacity 1) and then extended it with the step's operands, so the
second push reallocated: one `realloc` per element of the list, about 400
instructions each. The left, right-associative and scan forms now allocate the
accumulator plus the step's operands once.

On `sub f() { my @x = ^1000; ...; $sum += [+] (^1000).map({ @x[$_] }) }`
(callgrind slope, profiling build, warmed, N 20 to 120): 6,304 to 5,867
instructions per `.map` block call (-437, -6.9%), which also brings #16642's
celled-capture slope under its 6,065 target.
