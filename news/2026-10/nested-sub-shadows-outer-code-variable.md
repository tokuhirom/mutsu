# A routine's own nested sub shadows an outer `my &h`

`my &h = sub { 'outer' }; sub f { sub h { 'inner' }; h() }` now makes `f()`
answer `inner`, as in rakudo; the same holds for a nested `my sub h`. The bare
call was compiled as a call through the code variable `&h` whenever any
enclosing scope bound one, without asking whether a nearer scope declared a
routine of that name (#13855). A `sub h` declared directly in a mainline block
still loses to an outer `my &h`; that part of #13855 is open.
