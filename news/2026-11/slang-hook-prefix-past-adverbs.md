# A slang token's literal prefix is read past its regex adverbs

A slang production is tried at every statement start whose text begins with
the literal the token opens with (`token routine-declarator:sym<aggsub>
{ 'aggsub' ... }`). The slang host hands the parser the token body as
`:ratchet 'aggsub' ...`, and the leading adverb hid the literal, so no hook had a
prefix and every one ran the host's grammar at every statement of the unit and
of every module it loaded: about 55 ms a statement under Qwiratry's mold slang
(300 plain statements took 19 s, now 5 s of mostly module loading), and
`use Qwiratry::Transformer` after `use Qwiratry::Mold::Slang` did not finish.
The adverbs are skipped now (a case-insensitive `:i` literal still has no
prefix). Together with `Routine.meta-object` answering the closure of a
slang-built `Sub` / `Method` node (it raised "EVAL does not yet support
lowering `RakuAST::Method`"), Qwiratry's `mold` declarations compile (#13370, #17243).
