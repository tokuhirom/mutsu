# A lone `proto sub infix:<op>` now declares the operator

`proto sub infix:<precedes>($, $) {*}` without a following `multi` used to leave the
operator unknown to the parser, so `$a precedes $b` died with "Two terms in a row".
`proto_decl` now registers the sub name like `sub_decl_body` does. Found through the
`BinaryHeap` ecosystem distribution, whose `BinaryHeap` module now loads under mutsu.
