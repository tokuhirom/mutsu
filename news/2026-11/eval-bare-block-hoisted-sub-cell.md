# A sub in a bare block of an EVAL unit closes over the block's lexicals

`EVAL '{ say foo(); say foo(); my $a; sub foo { $a++ } }'` printed `0 0` where rakudo prints
`0 1`: a named sub declared in a bare block of an `EVAL` unit that runs at the program's top level
got no capture bucket (`exec_register_sub_op` excluded every `EVAL` unit from the block-bucket
arm), so its free `my` fell back to the flat env. A block of such a unit now gets its own
`UNIT<block ...>` bucket like a block of the program (`in_block_of_eval_unit`). About 23 `t/` files
that failed as `EVAL(slurp($file).AST)` for this reason join the ratchet.
`t/vm/scope/eval-bare-block-hoisted-sub-cell.t` pins it.
