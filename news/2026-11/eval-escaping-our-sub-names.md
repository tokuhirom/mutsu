# An our sub in a block of an EVAL unit closes over the block's lexicals

An `our sub` declared in a block of an `EVAL` unit read its free `my` variable as undefined once
the block had run: the names whose cells must outlive their block
(`escaping_our_lexical_names`) were collected only from the program's own compiled mainline, never
from an `EVAL` unit's code. The unit's compiled code now seeds them too
(`collect_escaping_our_lexical_names` in `compile_block_carrier`, for the unit's run only: a later unit's same-named variable must not resolve through them), so the `our sub` keeps the
block's cell as in the program itself. Six `t/` files that failed as `EVAL(slurp($file).AST)` join
the ratchet; `t/routines/call/eval-our-sub-block-lexical-capture.t` pins it.
