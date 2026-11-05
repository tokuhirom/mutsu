# A sub in a `do` block of an EVAL unit closes over the block's lexicals

`EVAL 'do { say foo(); say foo(); my $a; sub foo { $a++ } }'` printed `0 0` where rakudo prints
`0 1` (#16694). A `do` block pushes no block routine frame, so the follow-up of #16693 missed it: a
sub declared in a block nested below an `EVAL` unit's top level (by block depth, not only by routine
frame) now gets the block's own capture bucket (`in_block_of_eval_unit`).
`t/lang/eval-bare-block-hoisted-sub-cell.t` pins both forms.
